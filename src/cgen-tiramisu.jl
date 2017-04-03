using CompilerTools.LambdaHandling
using CompilerTools.Helper

jToC = Dict(
            Int8    =>  "int8_t",
            UInt8   =>  "uint8_t",
            Int16   =>  "int16_t",
            UInt16  =>  "uint16_t",
            Int32   =>  "int32_t",
            UInt32  =>  "uint32_t",
            Int64   =>  "int64_t",
            UInt64  =>  "uint64_t",
            Float16 =>  "float",
            Float32 =>  "float",
            Float64 =>  "double",
            Bool    =>  "bool",
            Char    =>  "char",
            Void    =>  "void",
            H5SizeArr_t => "hsize_t*",
            SizeArr_t => "uint64_t*"
    )

jToT = Dict(
            Int8    =>  "p_int8",
            UInt8   =>  "p_uint8",
            Int16   =>  "p_int16",
            UInt16  =>  "p_uint16",
            Int32   =>  "p_int32",
            UInt32  =>  "p_uint32",
            Int64   =>  "p_int64",
            UInt64  =>  "p_uint64",
            Float16 =>  "p_float32",
            Float32 =>  "p_float32",
            Float64 =>  "p_float64",
            Bool    =>  "p_boolean"
    ) 

computationIndex = 0

const HALIDE_INCLUDE_DIR = "/home/david/tiramisu/Halide/include"
const HALIDE_TOOLS_DIR = "/home/david/tiramisu/Halide/tools"
const TIRAMISU_INCLUDE_DIR = "/home/david/tiramisu/include"
const TIRAMISU_BUILD_DIR = "/home/david/tiramisu/build"
const FILENAME = "/home/david/generated/thello.cpp"
const GEN = "$TIRAMISU_BUILD_DIR/thello_generator"
const OBJ = "$TIRAMISU_BUILD_DIR/thello.o"
function_name = ""

type TBuffer
    #buffer buf0("buf0", 2, {tiramisu::expr(10),tiramisu::expr(10)}, p_uint8, NULL, a_output, &function0);
    name
    dimensions :: Array{Any,1}
    eltype
end

type TExpr
    name
    val
    typ
end

type TOp
    fcn
    args
end

type TRange
    iterator
    low
    high
end

type TConstant
    #constant N("N", e_N, p_int32, true, NULL, 0, &function0);
    name
    expr :: TExpr
    eltype
end

type TComputation
    #computation S0("[N]->{S0[i,j]: 0<=i<N and 0<=j<N}", e3, true, p_uint8, &function0);
    name
    space
    ranges :: Array{TRange}
    expr :: TExpr
    eltype
    access
end

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

function tiramisu_set_function(f)
    global function_name
    function_name = f
    s = string("int main()\n{\n",
                "\tglobal::set_default_tiramisu_options();\n",
                "\tfunction $function_name(\"$function_name\");\n")
end

function tiramisu_from_expr(ast::Expr, linfo)
    s = ""
    head = ast.head
    args = ast.args
    typ = ast.typ

    @dprintln(4, "from_expr = ", ast)
    if head == :body
        @dprintln(3,"Compiling body")
        newargs = tiramisu_transform_body(args,linfo)
        s *= tiramisu_translate(newargs)
        println(s)
    end
    s
end

function tiramisu_transform_body(args,linfo)
    newBody = Array{Any}(0)
    tempToValue = Dict{String,Any}()
    index = 1
    for i in 1:length(args)
        if index > i
            continue
        end
        println(3, "transform_body working on = ")
        if args[i].head == :(=)
            lhs = string(lookupVariableName(args[i].args[1],linfo))
            rhs = tiramisu_eval(args[i].args[2],linfo,tempToValue)
            if typeof(rhs) <: TBuffer
                rhs.name = lhs
            end
            tempToValue[lhs] = rhs
            push!(newBody,rhs)
            index += 1
        elseif args[i].head == :parfor_start
            for j in i:length(args)
                if args[j].head == :parfor_end
                    index = j
                    break
                end
            end
            newarg = tiramisu_eval_parfor(args[i:index],linfo,tempToValue)
            if isa(newarg.space,Number)
                con = tiramisu_eval_const(newarg.space)
                push!(newBody,con)
                newarg.space = con.name
            end
            push!(newBody,newarg)
        elseif args[i].head == :return
            push!(newBody,args[i]) #//set output here
        end
    end
    newBody
end

function tiramisu_eval(ast,linfo,ttv)
    if ast.head == :call
        tiramisu_eval_call(ast.args,ast.typ,linfo,ttv)
    #elseif ast.args[1].name == :sle_int
    #    tiramisu_eval_expr(args[2],linfo,ttv) <= tiramisu_eval_expr(args[3],linfo,ttv)
    #elseif ast.args[1].name == :sub_int
    #    tiramisu_eval_expr(args[2],linfo,ttv) - tiramisu_eval_expr(args[3],linfo,ttv)
    #elseif ast.args[1].name == :select_value
    #    tiramisu_eval_expr(args[2],linfo,ttv) ? tiramisu_eval_expr(args[3],linfo,ttv) : tiramisu_eval_expr(args[4],linfo,ttv)
    end
end

function tiramisu_eval_parfor(args,linfo,ttv)
    start = args[1]
    forend = args[end]
    exprs = args[3:end-2]
    comp = args[end-2]
    ln = start.args[1].loopNests
    ranges = Array{TRange,1}(length(ln))
    for i in 1:length(ln)
        iterator = string(lookupVariableName(ln[i].indexVariable,linfo))
        ranges[i] = TRange(iterator,ln[i].lower,ln[i].upper)
    end
    eltype = start.typ
    for e in exprs
        println("eeeeee")
        dump(e)
        dump(tiramisu_eval(e.args[2],linfo,ttv))
    end
    expr = TExpr("",comp.args[3],typeof(comp.args[3]))
    access = string(lookupVariableName(comp.args[2],linfo))
    TComputation("S0",ranges[1].high,ranges,expr,eltype,access)
end

function tiramisu_eval_call(args,typ,linfo,ttv)
    if args[1].name == :ccall
        dims = args[7:2:end]
        dimensions = Array{Any,1}(length(dims))
        for i in 1:length(dims)
            dimensions[i] = tiramisu_eval_expr(args[6+i],linfo,ttv)
        end
        TBuffer("",dimensions,typ.parameters[1])
    elseif args[1].name == :box
        tiramisu_eval_call(args[3].args,typ,linfo,ttv)
    elseif args[1].name == :add_int
        TExpr("",TOp("+",[tiramisu_eval_expr(args[2],linfo,ttv),tiramisu_eval_expr(args[3],linfo,ttv)]),typ.parameters[1])
    end
end

function tiramisu_eval_const(val)
    TConstant("N",TExpr("",val,typeof(val)),typeof(val))
end

function tiramisu_eval_expr(ast::Union{Int64},linfo,ttv)
    val = eval(ast)
    TExpr("",val,typeof(val))
end

function tiramisu_translate(statements)
    s = ""
    for stmt in statements
        s*=tiramisu_translate(stmt)
    end
    s
end

function tiramisu_translate(stmt::TConstant)
    #constant N("N", e_N, p_int32, true, NULL, 0, &function0);
    s = "\tconstant "
    s *= stmt.name*"(\""*stmt.name*"\", "
    s *= tiramisu_translate(stmt.expr)
    s *= ", "*jToT[stmt.eltype]*", true, NULL, 0, "
    s *= "&"*function_name*");\n"
    s
end

function tiramisu_translate(stmt::TExpr)
    s = ""
    if stmt.name != ""
        s *= "\texpr "*stmt.name* " = "
    end
    s*="expr("
    if stmt.val <: TOp
        s*=tiramisu_translate(stmt.val)
    else
        s*= "("*jToC[stmt.typ]*")"*string(stmt.val)
    end
    s*=")"
    s
end

function tiramisu_translate(stmt::TOp)
    tiramisu_translate(stmt.args[1])*" "*stmt.fcn*" "*tiramisu_translate(stmt.args[2])
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
    s *= "}, "*jToT[stmt.eltype]*", NULL, a_output, "
    s *= "&"*function_name*");\n"
    s
end

function tiramisu_translate(stmt::TComputation)
    #computation S0("[N]->{S0[i,j]: 0<=i<N and 0<=j<N}", e3, true, p_uint8, &function0);
    s = "\tcomputation " * stmt.name
    s *= "(\"["*string(stmt.space)*"]->{"*stmt.name*"["
    for r in stmt.ranges
        s *= r.iterator *","
    end
    s = s[1:length(s)-1]
    s *= "]: "
    for r in stmt.ranges
        s*= string(r.low)*"<="*r.iterator*"<"*string(r.high)*" and "
    end
    s = s[1:length(s)-5]
    s *= "}\", "*tiramisu_translate(stmt.expr)*", true, "
    s *= jToT[stmt.eltype]*", &"*function_name*");\n"
    #S0.set_access("{S0[i,j]->buf0[i,j]}");
    s *= "\t"*stmt.name*".set_access(\"{"
    s *= stmt.name * "["
    for r in stmt.ranges
        s *= r.iterator *","
    end
    s = s[1:length(s)-1]
    s *= "]->"*stmt.access*"["
    for r in stmt.ranges
        s *= r.iterator *","
    end
    s = s[1:length(s)-1]
    s *= "]}\");\n"
end

function tiramisu_translate(stmt::Expr)
    global function_name
    s = "\t$function_name.gen_time_processor_domain();\n"
    s *=  "\t$function_name.gen_isl_ast();\n"
    s *= "\t$function_name.gen_halide_stmt();\n"
    s *= "\t$function_name.gen_halide_obj(\"$OBJ\");\n"
    s*"\tprintf(\"Done\\n\");\n\treturn 0;\n"
end

function tiramisu_get_command()
    s = ["g++","-g","-std=c++11","-O3","-Wall","-Wno-sign-compare","-fno-rtti","-fvisibility=hidden"]
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
    push!(s,"-lpthread","-lz","`libpng-config --cflags --ldflags`","-ljpeg")
    Cmd(s)
end

function tiramisu_get_gen()
    Cmd(["$GEN"])
end

function tcanonicalize(tok)
    replacedTokens = Set("#")
    scrubbedTokens = Set(",.({}):")
    s = string(tok)
    s = replace(s, scrubbedTokens, "")
    s = replace(s, replacedTokens, "p")
    s = replace(s, "∇", "del")
    for (k,v) in tokenXlate
       s = replace(s, k, v)
    end
    s = replace(s, r"[^a-zA-Z0-9]", "_")
    s
end