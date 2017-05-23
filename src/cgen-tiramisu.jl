#
# A Julia to Tiramisu C++ Generator
# Transformation happens in two main steps:
#
#   - First, the Julia AST from ParallelAccelerator gets transformed
#     into a Tiramisu AST of TExpr's, TBuffer's, TComputation's, etc.
#
#   - Then, each Tiramisu AST node is passed into a tiramisu_translate
#     method to generate the proper Tiramisu C++ code.
#
# Optional passes could be added in the future in between the two steps 
# to perform other optimizations (eliminate redundant buffers, evaluate 
# gotos, etc)
#
# tiramisu_from_root_entry is the entry point where all the magic happens
# 
# Originally Created By David Vargas
#

module Tiramisu

import CompilerTools.DebugMsg
using CompilerTools.LambdaHandling
using CompilerTools.Helper
DebugMsg.init()

using ..ParallelAccelerator
export tiramisu_from_root_entry, tcanonicalize

jToC = Dict(
            Int8    =>  "int8_t",
            UInt8   =>  "uint8_t",
            Int16   =>  "int16_t",
            UInt16  =>  "uint16_t",
            Int32   =>  "int32_t",
            UInt32  =>  "uint32_t",
            Int64   =>  "int32_t",
            UInt64  =>  "uint32_t",
            Float16 =>  "float",
            Float32 =>  "float",
            Float64 =>  "double",
            Bool    =>  "bool",
            Char    =>  "char",
            Void    =>  "void"
    ) #Map from Julia types to C++ types

jToT = Dict(
            Int8    =>  "p_int8",
            UInt8   =>  "p_uint8",
            Int16   =>  "p_int16",
            UInt16  =>  "p_uint16",
            Int32   =>  "p_int32",
            UInt32  =>  "p_uint32",
            Int64   =>  "p_int32",
            UInt64  =>  "p_uint32",
            Float16 =>  "p_float32",
            Float32 =>  "p_float32",
            Float64 =>  "p_float64",
            Bool    =>  "p_boolean"
    ) #Map from Julia types to Tiramisu types

#TODO: Edit these constants to match the proper directory of all the following
const HALIDE_INCLUDE_DIR = "/home/david/tiramisu/Halide/include"
const HALIDE_TOOLS_DIR = "/home/david/tiramisu/Halide/tools"
const TIRAMISU_INCLUDE_DIR = "/home/david/tiramisu/include"
const TIRAMISU_BUILD_DIR = "/home/david/tiramisu/build"

#The following constants are the output files created
const FILENAME = "/home/david/generated/thello.cpp"
const GEN = "$TIRAMISU_BUILD_DIR/thello_generator"
const OBJ = "$TIRAMISU_BUILD_DIR/thello.o"
const LIB = "$TIRAMISU_BUILD_DIR/thello.so"

#Globals
function_name = ""
computationIndex = 0
computationPrefix = "S"

#The following types are what should be found on the top level of a Tiramisu AST
type TBuffer
    #buffer buf0("buf0", 2, {tiramisu::expr(10),tiramisu::expr(10)}, p_uint8, NULL, a_output, &function0);
    name
    dimensions :: Array{Any,1}
    typ
    isoutput
end

type TExpr
    #expr e_p0 = expr((int32_t) SIZE0);
    name
    val
    typ
end

type TConstant
    #constant N("N", e_N, p_int32, true, NULL, 0, &function0);
    name
    expr :: TExpr
    typ
end

type TComputation
    #computation S0("[N]->{S0[i,j]: 0<=i<N and 0<=j<N}", e3, true, p_uint8, &function0);
    name
    space
    ranges
    expr
    typ
    access
    setafter :: Bool
end

#The following types are found on lower levels of a Tiramisu AST to help translate,
#modify, and optimize the Tiramisu AST
type TOp
    fcn
    args
end

type TRange
    iterator
    low
    high
end

type TAccess
    buf
    ind
    val
end

type TSignature
    args
    ret
end

#IMPORTANT: This is the main entry point for Julia to Tiramisu Translation
#This functions performs a number of key steps:
#   -First it gets the header string which includes imports and the namespace
#   -Second it sets the name of the function, which declares the method signature,
#    setting Tiramisu default options, and declaring the function to be generated.
#   -Third it translates the body of the program using the steps outlined in the
#    beginning. This is where the majority of the work takes place
#   -Next it writes the combination of this string to a file 
#   -Then the file is compiled, run, and converted to a library file
#   -Finally, the library file name is returned to be used by Julia
function tiramisu_from_root_entry(body, linfo, functionName::AbstractString)

    #Form Tiramisu String
    thdr = tiramisu_from_header(linfo)
    twrap = tiramisu_set_function(functionName)
    tbod = tiramisu_from_expr(body,linfo)
    tc = thdr * twrap* tbod * "}\n"

    #Write to File
    fil = open(FILENAME,"w")
    write(fil, tc)
    close(fil)
    #println("Wrote to file")

    #Compile the Function Generator, Generate the function and link
    run(tiramisu_get_command())
    #println("Compiled Tiramisu File")
    run(tiramisu_get_gen())
    #println("Generated Function")
    run(tiramisu_get_lib())
    #println("Converted to .so file")
    LIB
end

#Imports for Tiramisu file
function tiramisu_from_header(linfo)
    s = string("#include <isl/set.h>\n",
        "#include <isl/union_map.h>\n",
        "#include <isl/union_set.h>\n",
        "#include <isl/ast_build.h>\n",
        "#include <isl/schedule.h>\n",
        "#include <isl/schedule_node.h>\n\n",

        "#include <$(TIRAMISU_INCLUDE_DIR)/tiramisu/debug.h>\n",
        "#include <$(TIRAMISU_INCLUDE_DIR)/tiramisu/core.h>\n\n",

        "#include <stdint.h>\n",
        "#include <float.h>\n",
        "#include <limits.h>\n",
        "#include <complex>\n",
        "#include <math.h>\n",
        "#include <stdio.h>\n",
        "#include <iostream>\n",
        "#include <sstream>\n",
        "#include <vector>\n",
        "#include <string>\n",
        "#include <Halide.h>\n\n",
        "using namespace tiramisu;\n\n")
end

#Method Signature for Tiramisu File
function tiramisu_set_function(f)
    global function_name
    global computationPrefix
    function_name = f
    computationPrefix = f*"_S"
    s = string("int main()\n{\n",
                "\tglobal::set_default_tiramisu_options();\n",
                "\tfunction $function_name(\"$function_name\");\n")
end

#Entry Point for Transforming and translating Julia AST body into body of
#Tiramisu program
function tiramisu_from_expr(ast::Expr, linfo)
    s = ""
    head = ast.head
    args = ast.args
    typ = ast.typ

    if head == :body
        newargs = tiramisu_transform_body(args,linfo) #Main step 1 from intro
        s *= tiramisu_translate(newargs) #Main step 2 from intro
    end
    s
end

#Entry Point for the first main step from the Intro. The job of this method is
#to convert a Julia AST into an array of TIramisu Nodes from the types outlined
#above. Note not all possible Julia nodes are supported.
function tiramisu_transform_body(args,linfo)
    #Uncomment loop below to see Julia ast nodes before converted to Tiramisu
    #for i in 1:length(args)
    #    println("transform iteration:", i)
    #    dump(args[i])
    #end

    #Start of method transformation
    newBody = Array{Any}(0)
    index = 0
    for i in 1:length(args)
        println("started iteration ",i," newBody size ",length(newBody))
        if index >= i
            continue
        end
        if typeof(args[i]) <: Union{LabelNode,GotoNode}
            #ignore
            index+=1
        elseif args[i].head == :(=)
            lhs = tiramisu_get_var(args[i].args[1],linfo)
            rhs = tiramisu_eval(args[i].args[2],linfo)
            if typeof(rhs) <: TExpr && typeof(rhs.val) <: TOp && startswith(rhs.val.fcn,"Matrix")
                rhs = tiramisu_eval_matrixop(rhs,linfo,newBody)
                rhs.access.val = lhs#TODO: This is a hack
            elseif typeof(rhs) <: Union{TExpr,TBuffer}
                rhs.name = lhs
            else
                rhs = TExpr(lhs,string(rhs),Nullable)
            end
            #=Hacky deprecated loop that was created for kmeans
            if rhs.typ <: SlotNumber
                for j in length(newBody):-1:1
                    buffercopy = rhs.val
                    if typeof(newBody[j]) <: TComputation && newBody[j].access.buf == buffercopy
                        rhs.val = newBody[j]
                        for k in j:-1:1
                            if typeof(newBody[k]) <: TBuffer && newBody[k].name == buffercopy
                                rhs.val.access.ind = newBody[k]
                                break
                            end
                        end
                        break
                    end 
                end
            end
            =#
            push!(newBody,rhs)
            index += 1
        elseif args[i].head == :parfor_start
            loopdepth = 0
            for j in i:length(args)
                if isdefined(args[j],:head) && args[j].head == :parfor_end
                    loopdepth -= 1
                end
                if isdefined(args[j],:head) && args[j].head == :parfor_start
                    loopdepth += 1
                end
                if loopdepth == 0
                    index = j
                    break
                end
            end
            newarg = tiramisu_eval_parfor(args[i:index],linfo)
            prevexprs = newarg[1]
            for j in 1:length(prevexprs)
                push!(newBody,prevexprs[j])
            end
            space = newarg[2][1].space
            for j in 1:length(space)
                con = tiramisu_eval_const(space[j],newarg[2][1].name,j)
                push!(newBody,con)
                for r in newarg[2][1].ranges
                    if r.high == space[j]
                        r.high = con.name
                    end
                end
                space[j] = con.name
            end
            newBody = vcat(newBody,newarg[2])
        elseif args[i].head == :return
            newarg = tiramisu_eval_return(args[i],linfo)
            push!(newBody,newarg)
            for i in length(newBody):-1:1
                if typeof(newBody[i]) <: TBuffer && newBody[i].name == newarg.ret
                    newBody[i].isoutput = true
                    break
                end
            end
            index+= 1
        elseif args[i].head == :call
            newarg = tiramisu_eval_call(args[i].args,args[i].typ,linfo)
            push!(newBody,newarg)
            index+= 1
        elseif args[i].head == :meta || args[i].head == :inbounds
            #ignore
            index += 1
        elseif args[i].head == :gotoifnot
            #ignore
            index += 1
        else
            dump(args[i])
            println("Tiramisu_transform_body error unknown head ",args[i].head)
        end
        #dump(newBody[end])
    end
    newBody
end

#The following set of tiramisu_eval_* functions are all used through various points
#of walking the Julia AST for the above tiramisu_transform_body method.

function tiramisu_eval(ast,linfo)
    if !isdefined(ast,:head)
        tiramisu_eval_expr(ast,linfo)
    elseif ast.head == :call
        tiramisu_eval_call(ast.args,ast.typ,linfo)
    end
end

function tiramisu_eval_parfor(args,linfo)
    global computationIndex
    global computationPrefix
    forstart = args[1]
    forend = args[end]
    exprs = args[2:end-1]
    ln = forstart.args[1].loopNests
    ranges = []
    for i in length(ln):-1:1
        iterator = tiramisu_get_var(ln[i].indexVariable,linfo)
        push!(ranges,TRange(iterator,ln[i].lower,ln[i].upper))
    end
    typ = forstart.typ
    newexprs = tiramisu_transform_body(exprs,linfo)
    space = []
    for r in ranges
        if !(r.high in space)
            push!(space,r.high)
        end
    end
    access = []
    for i in length(newexprs):-1:1
        if typeof(newexprs[i]) <: TAccess
            push!(access,newexprs[i])
            deleteat!(newexprs,i)
        end
    end
    tc = []
    for i in length(access):-1:1
        a = access[i]
        compexpr = TExpr("",nothing,Nullable)
        compname = computationPrefix*string(computationIndex)
        computationIndex += 1
        if length(newexprs) > 0
            compexpr = newexprs[end]
            compexpr = TExpr("",compexpr.val,compexpr.typ)
        elseif a.val != nothing
            compexpr = TExpr("",a.val,typeof(a.val))
        end
        push!(tc,TComputation(compname,space,ranges,compexpr,typ,a,false))
    end
    if length(newexprs)>0
        return [newexprs,tc]
    else
        return [[],tc]
    end
end

function tiramisu_eval_return(arg,linfo)
    ret = tiramisu_get_var(arg.args[1],linfo)
    params = linfo.input_params
    sig = TSignature(params,ret)
    if length(params) == 0
        push!(sig.args,sig.ret)
    else
        #TODO set args to params
    end
    sig
end

function tiramisu_eval_call(args,typ,linfo)
    if args[1].name == :ccall
        dims = args[7:2:end]
        dimensions = Array{Any,1}(length(dims))
        for i in 1:length(dims)
            dimensions[i] = tiramisu_eval(dims[i],linfo)
        end
        TBuffer("",dimensions,typ.parameters[1],false)
    elseif args[1].name == :box
        tiramisu_eval_call(args[3].args,typ,linfo)
    elseif args[1].name == :checked_trunc_sint
        TExpr("",args[3],args[2])
    elseif args[1].name == :+ || args[1].name == :add_int || args[1].name == :checked_sadd_int
        TExpr("",TOp("+",[tiramisu_eval_expr(args[2],linfo), tiramisu_eval_expr(args[3],linfo)]),typ)
    elseif args[1].name == :sle_int || args[1].name == :lt_float
        TExpr("",TOp("<",[tiramisu_eval_expr(args[2],linfo), tiramisu_eval_expr(args[3],linfo)]),typ)
    elseif args[1].name == :xor_int
        TExpr("",TOp("^",[tiramisu_eval_expr(args[2],linfo), tiramisu_eval_expr(args[3],linfo)]),typ)
    elseif args[1].name == :div_float
        TExpr("",TOp("/",[tiramisu_eval_expr(args[2],linfo), tiramisu_eval_expr(args[3],linfo)]),typ)
    elseif args[1].name == :not_int
        TExpr("",TOp("!",[tiramisu_eval_expr(args[2],linfo)]),typ)
    elseif args[1].name == :flipsign_int
        TExpr("",TOp("-",[tiramisu_eval_expr(args[2],linfo)]),typ)
    elseif args[1].name == :(===)
        TExpr("",TOp("==",[tiramisu_eval_expr(args[2],linfo), tiramisu_eval_expr(args[3],linfo)]),typ)
    elseif args[1].name == :arraysize
        TExpr("",50000,Int64) #TODO: Hardcoded adjust this
    elseif args[1].name == :sub_int || args[1].name == :sub_float || args[1].name == :checked_ssub_int
        TExpr("",TOp("-",[tiramisu_eval_expr(args[2],linfo), tiramisu_eval_expr(args[3],linfo)]),typ)
    elseif args[1].name == :select_value
        TExpr("",1,Int64) #TODO: Fix
    elseif args[1].name == :rand
        TExpr("","((double)rand()/(RAND_MAX))",Float64)
    elseif args[1].name == :sqrt
        TExpr("",TOp("sqrt(",[tiramisu_eval_expr(args[2],linfo)]),typ)
    elseif args[1].name == :powi_llvm
        TExpr("",TOp("pow(",[tiramisu_eval_expr(args[2],linfo), tiramisu_eval_expr(args[3],linfo)]),typ)
    elseif args[1].name == :unsafe_arrayref ||  args[1].name == :arrayref
        ref = tiramisu_get_var(args[2],linfo)*"("*join(map(x->"var(\""*tiramisu_get_var(x,linfo)*"\")",args[3:end]),",")*")"
        TExpr("",ref,SSAValue)
    elseif args[1].name == :unsafe_arrayset || args[1].name == :arrayset
        tiramisu_eval_access(args[2:end],linfo)
    elseif args[1].name == :getfield || args[1].name == :setfield! 
        TExpr("",1,Int64) #TODO: Fix
    elseif args[1].name == :arraylen
        TExpr("",TOp("get_dim_sizes()",[tiramisu_eval_expr(args[2],linfo)]),typ) #TODO: Fix
    elseif args[1].name == :sitofp
        TExpr("",tiramisu_eval_expr(args[3],linfo),args[2])
    elseif args[1].name == :generic_matmatmul!
        TExpr("",TOp("Matrix*",args[2:end]),typ)
    else
        println("Tiramisu_eval_call error unknown name ",args[1].name)
        dump(args)
        TExpr("",args[3],args[2])
    end
end

function tiramisu_eval_const(val,compname,i)
    TConstant(compname*"N"*string(i),TExpr("",val,typeof(val)),typeof(val))
end

function tiramisu_eval_expr(ast::Union{Int64,Int32,Float64,Float32,Float16,Int16,Int8},linfo)
    val = eval(ast)
    TExpr("",val,typeof(val))
end

function tiramisu_eval_expr(ast::Union{SlotNumber,SSAValue,TypedSlot},linfo)
    TExpr("",tiramisu_get_var(ast,linfo),typeof(ast))
end

function tiramisu_eval_access(args,linfo)
    ta = TAccess("",[],nothing)
    ta.buf = tiramisu_get_var(args[1],linfo)
    if typeof(args[2]) <: Union{Int64,Int32,Float64,Float32,Float16,Int16,Int8}
        ta.val = args[2]
    else
        ta.val = tiramisu_get_var(args[2],linfo)
    end
    ta.ind = map(x -> tiramisu_get_var(x,linfo),args[3:end])
    ta
end

function tiramisu_eval_matrixop(ast, linfo, newBody)
    global computationIndex
    global computationPrefix
    op = ast.val.fcn[7:end]
    outputBufName = tiramisu_get_var(ast.val.args[1],linfo)
    inputBufNames = []
    for arg in ast.val.args[2:end]
        if typeof(arg) <: Union{SSAValue,SlotNumber}
            push!(inputBufNames,tiramisu_get_var(arg,linfo))
        end
    end
    inputComps = []
    for i in length(newBody):-1:1
        if typeof(newBody[i]) <: TComputation && newBody[i].access.buf in inputBufNames
            insert!(inputComps,1,newBody[i])
        end
        if length(inputComps) == length(inputBufNames)
            break
        end
    end
    compname = computationPrefix*string(computationIndex)
    computationIndex += 1
    if op == "*"
        space = inputComps[1].space
        ranges = copy(inputComps[1].ranges)
        tempIndex = compname*"_i"
        push!(ranges,TRange(tempIndex,ranges[1].low,ranges[1].high))
        left = inputComps[1]
        right = inputComps[2]
        compexprOp = TOp("*",[left.name*"(var(\""*left.ranges[1].iterator*"\"),var(\""*tempIndex*"\"))",right.name*"(var(\""*tempIndex*"\"),var(\""*right.ranges[2].iterator*"\"))"])
        compexpr = TExpr(compname*"_e",compexprOp,ast.typ)
        access = TAccess(outputBufName,map(x->x.iterator,left.ranges),nothing)
        outputComp = TComputation(compname,space,ranges,compexpr,left.typ,access,true)
        return outputComp
    end
end

#The end of tiramisu_eval_* methods needed for AST transformation

#Entry point for the second main step from the Intro. Takes the array of 
#Tiramisu nodes and translates each one into its proper c++ code. There
#should be no AST manipulation after this point, just read and translation. 
function tiramisu_translate(statements::Array)
    #Uncomment loop below to see each Tiramisu node before string translation
    #for i in 1:length(statements)
    #    println("translate iteration:",i)
    #    dump(statements[i])
    #end

    #Beginning of translation
    s = ""
    for i in 1:length(statements)
        stmt = statements[i]
        #println("iteration:",i)
        #println(tiramisu_translate(stmt))
        #s *= "\tprintf(\"Done$i\\n\");\n" #Uncomment this line to add debug lines in Tiramisu file
        s*=tiramisu_translate(stmt)
    end
    s
end

function tiramisu_translate(stmt::TConstant)
    #constant N("N", e_N, p_int32, true, NULL, 0, &function0);
    s = "\tconstant "
    s *= stmt.name*"(\""*stmt.name*"\", "
    s *= tiramisu_translate(stmt.expr)
    s *= ", "*jToT[stmt.typ]*", true, NULL, 0, "
    s *= "&"*function_name*");\n"
    s
end

function tiramisu_translate(stmt::TExpr)
    s = ""
    if typeof(stmt.val) <: TComputation
        tb = stmt.val.access.ind
        tb.name = stmt.name
        s *= tiramisu_translate(tb)
        stmt.val.access.buf = stmt.name
        return s*tiramisu_translate_access(stmt.val)
    end
    if stmt.name != ""
        s *= "\texpr "*stmt.name* " = "
    end
    if stmt.typ <: Union{SSAValue,SlotNumber,String}
        s *= stmt.val
    else
        s*="expr("
        if typeof(stmt.val) <: Union{TOp,TExpr}
            s *=tiramisu_translate(stmt.val)
        elseif !(stmt.typ <: Nullable)
            s *= "("*jToC[stmt.typ]*")"*string(stmt.val)
        elseif stmt.val != nothing
            s *= stmt.val
        end
        s*=")"
    end
    if stmt.name != ""
        s *= ";\n"
    end
    s
end

function tiramisu_translate(stmt::TOp)
    if endswith(stmt.fcn,"()")
        tiramisu_get_var(stmt.args[1])*"."*stmt.fcn
    elseif endswith(stmt.fcn,"(")
        s = stmt.fcn
        for a in stmt.args
            s *= tiramisu_translate(a) * ","
        end
        s = s[1:length(s) - 1]
        s *= ")"
    elseif length(stmt.args) == 1
        stmt.fcn*tiramisu_translate(stmt.args[1])
    elseif length(stmt.args) == 2
        tiramisu_translate(stmt.args[1])*" "*stmt.fcn*" "*tiramisu_translate(stmt.args[2])
    else
        println("Unknown TOp translation")
        dump(stmt)
        ""
    end
end

function tiramisu_translate(stmt::TBuffer)
#buffer buf0("buf0", 2, {tiramisu::expr(10),tiramisu::expr(10)}, p_uint8, NULL, a_output, &function0);
    s = "\tbuffer "
    s *= stmt.name
    s *= "(\""*stmt.name*"\", "
    s *= string(length(stmt.dimensions)) *", {"
    for d in stmt.dimensions
        s *= tiramisu_translate(d)*", "
    end
    s = s[1:length(s)-2]
    s *= "}, "*jToT[stmt.typ]*", NULL, "
    if stmt.isoutput
        s *= "a_output"
    else
        s *= "a_temporary"
    end
    s *= ", &"*function_name*");\n"
    s
end

function tiramisu_translate(stmt::TComputation)
    #computation S0("[N]->{S0[i,j]: 0<=i<N and 0<=j<N}", e3, true, p_uint8, &function0);
    s = "\tcomputation " * stmt.name
    s *= "(\"["
    for r in stmt.space
        s *= string(r)*","
    end
    s = s[1:length(s)-1]
    s *= "]->{"*stmt.name*"["
    for r in stmt.ranges
        s *= r.iterator *","
    end
    s = s[1:length(s)-1]
    s *= "]: "
    for r in stmt.ranges
        s*= string(r.low)*"<="*r.iterator*"<"*string(r.high)*" and "
    end
    s = s[1:length(s)-5]
    s *= "}\", "
    if stmt.setafter
        s *= tiramisu_translate(TExpr("",nothing,Nullable))    
    else
        s *= tiramisu_translate(stmt.expr)
    end
    s *= ", true, "*jToT[stmt.typ]*", &"*function_name*");\n"
    if stmt.setafter
        s *= tiramisu_translate(stmt.expr)
        s *= "\t"*stmt.name*".set_expression("*stmt.expr.name*");\n"
        #c_C.set_expression(e1);
    end
    #S0.set_access("{S0[i,j]->buf0[i,j]}");
    s *= tiramisu_translate_access(stmt)
end

function tiramisu_translate(stmt::TSignature)
    global function_name
    s = "\t$function_name.set_arguments({&"*string(stmt.ret)*"});\n" 
    s *= "\t$function_name.gen_time_processor_domain();\n"
    s *=  "\t$function_name.gen_isl_ast();\n"
    s *= "\t$function_name.gen_halide_stmt();\n"
    s *= "\t$function_name.gen_halide_obj(\"$OBJ\");\n"
    s*"\treturn 0;\n"
end

function tiramisu_translate(stmt::TAccess)
    ""
end

function tiramisu_translate(stmt::String)
    stmt
end

function tiramisu_translate_access(stmt::TComputation)
    #S0.set_access("{S0[i,j]->buf0[i,j]}");
    s = "\t"*stmt.name*".set_access(\"{"
    s *= stmt.name * "["
    for r in stmt.ranges
        s *= r.iterator *","
    end
    s = s[1:length(s)-1]
    s *= "]->"*stmt.access.buf*"["
    for r in stmt.access.ind
        s *= r *","
    end
    s = s[1:length(s)-1]
    s *= "]}\");\n"
    if stmt.setafter#TODO: This is a hack
        s *= "\tcomputation "*stmt.access.val*" = "*stmt.name*";\n"
    end 
    s
end

#End of tiramisu_translate methods

#Returns the command to compile the written Tiramisu file
function tiramisu_get_command()
    s = ["g++","-g","-std=c++11","-O3","-Wall","-Wno-sign-compare","-fno-rtti","-fvisibility=hidden","-fPIC"]
    push!(s,"$TIRAMISU_BUILD_DIR/tiramisu_core.o")
    push!(s,"$TIRAMISU_BUILD_DIR/tiramisu_codegen_halide.o")
    push!(s,"$TIRAMISU_BUILD_DIR/tiramisu_codegen_c.o")
    push!(s,"$TIRAMISU_BUILD_DIR/tiramisu_debug.o")
    push!(s,"$TIRAMISU_BUILD_DIR/tiramisu_utils.o")
    push!(s,"$TIRAMISU_BUILD_DIR/tiramisu_codegen_halide_lowering.o")
    push!(s,"$TIRAMISU_BUILD_DIR/tiramisu_codegen_from_halide.o")
    push!(s,"$FILENAME","-o","$GEN","-I$TIRAMISU_INCLUDE_DIR") 
    push!(s,"-I/usr/local/include","-I$HALIDE_INCLUDE_DIR")
    push!(s,"-I$HALIDE_TOOLS_DIR","-I$TIRAMISU_BUILD_DIR")
    push!(s,"-L/usr/local/lib","-lisl","-lgmp","-L$HALIDE_INCLUDE_DIR/../lib","-lHalide","-ldl")
    push!(s,"-lpthread","-lz","-ljpeg")#`libpng-config --cflags --ldflags`","-ljpeg")
    Cmd(s)
end

#Returns the command to run the function generator that was compiled by the Tiramisu file
function tiramisu_get_gen()
    env = copy(ENV)
    if haskey(env, "LD_LIBRARY_PATH") && env["LD_LIBRARY_PATH"] != ""
        env["LD_LIBRARY_PATH"]*= ":$HALIDE_INCLUDE_DIR/../lib:/usr/local/lib:$TIRAMISU_BUILD_DIR"
    else
        env["LD_LIBRARY_PATH"] = "$HALIDE_INCLUDE_DIR/../lib:/usr/local/lib:$TIRAMISU_BUILD_DIR"
    end
    if haskey(env, "DYLD_LIBRARY_PATH") && env["DYLD_LIBRARY_PATH"] != ""
        env["DYLD_LIBRARY_PATH"]*= ":$HALIDE_INCLUDE_DIR/../lib:$TIRAMISU_BUILD_DIR"
    else
        env["DYLD_LIBRARY_PATH"] = "$HALIDE_INCLUDE_DIR/../lib:$TIRAMISU_BUILD_DIR"
    end
    s = Cmd(["$GEN"])
    setenv(s,env)
end

#Returns the command to convert the generated function from an object file to a 
#dynamic shared library file. This is necessary because Julia could only use c++
#code stored in libraries as opposed to object files 
function tiramisu_get_lib()
    s = ["g++","-g","-std=c++11","-O3","-Wall","-Wno-sign-compare","-fno-rtti","-fvisibility=hidden","-fPIC"]
    push!(s,"-shared","-fPIC","-o","$LIB","$OBJ")
    push!(s,"$TIRAMISU_BUILD_DIR/tiramisu_core.o")
    push!(s,"$TIRAMISU_BUILD_DIR/tiramisu_codegen_halide.o")
    push!(s,"$TIRAMISU_BUILD_DIR/tiramisu_codegen_c.o")
    push!(s,"$TIRAMISU_BUILD_DIR/tiramisu_debug.o")
    push!(s,"$TIRAMISU_BUILD_DIR/tiramisu_utils.o")
    push!(s,"$TIRAMISU_BUILD_DIR/tiramisu_codegen_halide_lowering.o")
    push!(s,"$TIRAMISU_BUILD_DIR/tiramisu_codegen_from_halide.o")
    push!(s,"-I$TIRAMISU_INCLUDE_DIR")
    push!(s,"-I/usr/local/include","-I$HALIDE_INCLUDE_DIR")
    push!(s,"-I$HALIDE_TOOLS_DIR","-I$TIRAMISU_BUILD_DIR")
    push!(s,"-L/usr/local/lib","-lisl","-lgmp","-L$HALIDE_INCLUDE_DIR/../lib","-lHalide","-ldl")
    push!(s,"-lpthread","-lz","-ljpeg", "-ldl")#`libpng-config --cflags --ldflags`","-ljpeg")
    Cmd(s)
end

#Canonicalizing method used to remove #'s and other undesirable chars from function name
function tcanonicalize(tok)
    replacedTokens = Set("#")
    scrubbedTokens = Set(",.({}):")
    tokenXlate = Dict(
        '*' => "star",
        '/' => "slash",
        '-' => "minus",
        '!' => "bang",
        '.' => "dot",
        '^' => "hat",
        '|' => "bar",
        '&' => "amp",
        '=' => "eq",
        '\\' => "backslash"
    )
    s = string(tok)
    s = replace(s, scrubbedTokens, "")
    s = replace(s, replacedTokens, "t")
    s = replace(s, "∇", "del")
    for (k,v) in tokenXlate
       s = replace(s, k, v)
    end
    s = replace(s, r"[^a-zA-Z0-9]", "_")
    s
end

#Converts a Julia ast that refers to a variable name into the proper variable name
#This is the main reason we pass linfo around throughout body transfomation in
#in main step 1.
function tiramisu_get_var(ast,linfo)
    s = string(lookupVariableName(ast,linfo))
    s = tcanonicalize(s)
    replace(s,Set("()"),"")
end

end