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
            Int64   =>  "int64_t",
            UInt64  =>  "uint64_t",
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
            Int64   =>  "p_int64",
            UInt64  =>  "p_uint64",
            Float16 =>  "p_float32",
            Float32 =>  "p_float32",
            Float64 =>  "p_float64",
            Bool    =>  "p_boolean"
    ) #Map from Julia types to Tiramisu types

Primitive = Union{Int8, UInt8, Int16, UInt16, Int32, UInt32, Int64, UInt64,
                  Float16, Float32, Float64, Bool}


#TODO: Edit these constants to match the proper directory of all the following
const HALIDE_INCLUDE_DIR = "/home/malek/tiramisu/Halide/include"
const HALIDE_TOOLS_DIR = "/home/malek/tiramisu/Halide/tools"
const HALIDE_LIB_DIR = "/home/malek/tiramisu/Halide/lib"
const ISL_INCLUDE_DIR = "/home/malek/tiramisu/isl/build/include"
const ISL_LIB_DIR = "/home/malek/tiramisu/isl/build/lib"
const TIRAMISU_INCLUDE_DIR = "/home/malek/tiramisu/include"
const TIRAMISU_BUILD_DIR = "/home/malek/tiramisu/build"

#The following constants are the output files created
const FILENAME = "/home/malek/generated/thello.cpp"
const FILENAME_WRAPPER = "/home/malek/generated/thello_wrapper.cpp"
const GEN = "$TIRAMISU_BUILD_DIR/thello_generator"
const OBJ = "$TIRAMISU_BUILD_DIR/thello.o"
const OBJ_WRAPPER = "$TIRAMISU_BUILD_DIR/thello_wrapper.o"
const LIB = "$TIRAMISU_BUILD_DIR/thello.so"

#Globals
function_name = ""
computationIndex = 0
computationPrefix = "S"

@enum TBufferArgument TBOutput TBInput TBTemporary
argNames = Dict(
    TBOutput => "a_output",
    TBInput => "a_input",
    TBTemporary => "a_temporary",
)
@enum TOpStyle TOpFuncCall TOpInfix

abstract TExpr

type TEmptyExpr <: TExpr
end

type TValue <: TExpr
    #expr e_p0 = expr((int32_t) SIZE0);
    name::String
    val
    typ::DataType
end

#The following types are found on lower levels of a Tiramisu AST to help translate,
#modify, and optimize the Tiramisu AST
type TOp <: TExpr
    name::String
    fcn::String
    style::TOpStyle
    args::Array{TExpr, 1}
    typ::DataType
end

#The following types are what should be found on the top level of a Tiramisu AST
type TBuffer
    #buffer buf0("buf0", 2, {tiramisu::expr(10),tiramisu::expr(10)}, p_uint8, NULL, a_output, &function0);
    name
    dimensions :: Array{TExpr,1}
    typ
    argument::TBufferArgument
end



type TConstant
    #constant N("N", e_N, p_int32, true, NULL, 0, &function0);
    name
    expr :: TExpr
    typ
end

indexNames = "ijklmnop"

type ISLCondition
    var::String
    op::String
    lim::String
end

type TComputation
    #computation S0("[N]->{S0[i,j]: 0<=i<N and 0<=j<N}", e3, true, p_uint8, &function0);
    name::String
    conditions::Array{ISLCondition}
    dim::Unsigned
    expr::TExpr
    typ::DataType
#    access
    is_input::Bool
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

type JVar
    typ::DataType
    buffer::TBuffer
    computation::TComputation
end

abstract TIndex

type TConstIndex <: TIndex
    val::Integer
end

type TVarIndex <: TIndex
    letter::Unsigned
end

type TRead <: TExpr
    var::JVar
    indices::Vector{TIndex}
end

vars = Dict{SSAValue, JVar}()

typToTBuffer = Dict(
    Int64 => function(id, typ)
        return TBuffer("B"*string(id.id),
                       [TValue("", 1, Int64)],
                       Int64,
                       TBTemporary)
    end,
)

function make_counter()
    counter = -1
    return function()
        counter += 1
        return counter
    end
end

computation_counter = make_counter()
new_computation_id() = "S"*string(computation_counter())

function parseExpr(expr::Expr)
    return exprHandlers[expr.head](expr)
end

function parseComputation(expr::Primitive)
    return TComputation(
        new_computation_id(),
        [ISLCondition("i", "=", "0")],
        1,
        getRHS(expr),
        typeof(expr),
        false # TODO : fix
    )
end

function getRHS(node::Primitive)
    return TValue("", node, typeof(node))
end

function getRHS(node::SSAValue)
    if !haskey(vars, node)
        throw(UndefVarError(Symbol(node)))
    end
    return TRead(vars[node], [TConstIndex(0)])
end


callHandlers = Dict(
    (Core.Intrinsics, :add_int) => function(args, typ)
        return TComputation(
            new_computation_id(),
            [ISLCondition("i", "=", "0")],
            1,
            TOp(
                "",
                "+",
                TOpInfix,
                [getRHS(arg) for arg in args],
                typ
            ),
            typ,
            false # TODO : fix
        )
    end,
    (Core.Intrinsics, :box) => function(args, typ)
        return parseExpr(args[2])
    end
)

function handleReturn(arg::SSAValue, typ::DataType)
    vars[expr.args[1]].buffer.argument = TBOutput
end

function handleReturn(arg, typ::DataType)
    id = SSAValue(length(vars))
    buffer = typToTBuffer[typ](id, typ)
    buffer.argument = TBOutput
    comp = parseComputation(arg)
    buffer.argument = TBOutput
    vars[id] = JVar(typ, buffer, comp)
end

exprHandlers = Dict(
    # In case of call, lookup the function in the list of supported functions
    :call => function(expr)
        called_function_id = (expr.args[1].mod, expr.args[1].name)
        return get(callHandlers,
                   called_function_id,
                   function(args) end)(expr.args[2:end], expr.typ)
    end,

    # In case of assignment, create buffer, computation
    :(=) => function(expr)
        id = expr.args[1]
        typ = expr.typ

        vars[id] = JVar(
            typ,
            typToTBuffer[typ](id, typ),
            parseExpr(expr.args[2]),
        )
    end,

    # For return, set buffer to output
    :return => function(expr)
        assert(length(expr.args) == 1) # TODO support tuples
        handleReturn(expr.args[1], expr.typ)
    end,
)

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
    tbod = tiramisu_analyze_body(body, linfo)
    tc = thdr * twrap* tbod * "}\n"

    tcw = generate_wrapper()


    println(functionName)

    #Write to File
    fil = open(FILENAME,"w")
    write(fil, tc)
    close(fil)
    #println("Wrote to file")

    #Write to File
    fil = open(FILENAME_WRAPPER,"w")
    write(fil, tcw)
    close(fil)
    #println("Wrote to file")

    #Compile the Function Generator, Generate the function and link
    run(tiramisu_get_command())
    #println("Compiled Tiramisu File")
    run(tiramisu_get_gen())
    #println("Generated Function")
    run(tiramisu_get_lib_obj())
    run(tiramisu_get_lib())
    #println("Converted to .so file")
    LIB #, get_return()
end

# function get_return()
#     return Vector{}
# end

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

getIndex(index::TConstIndex) = string(index.val)

getIndex(index::TVarIndex) = indexNames[index.letter]

getIndices(indices::Vector{TIndex}) = join(map(getIndex, indices), ", ")

createTExpr(val::TValue) = "expr(($(jToC[val.typ])) $(val.val))"
function createTExpr(var::TRead)
    "$(var.var.computation.name)(" * getIndices(var.indices) * ")"
end
function createTExpr(op::TOp)
    if op.style == TOpInfix
        return join(map(createTExpr, op.args), op.fcn)
    end
    return op.fcn * "(" * join(map(createTExpr, op.args), ",") * ")"
end

function createISLCond(cond::ISLCondition)
    return "$(cond.var) $(cond.op) $(cond.lim)"
end

function getIndices(comp::TComputation)
    return join(indexNames[1:comp.dim], ", ")
end

function createISL(comp::TComputation)
    indices_used = getIndices(comp)
    return "{$(comp.name)[$indices_used] : " * join(
        map(createISLCond, comp.conditions), " and ") * "}"
end

function tiramisu_analyze_body(ast::Expr, linfo)
    for expr in ast.args
        dump(expr)
        parseExpr(expr)
    end
    # computation pass
    lines = Vector{String}()
    for (id, var) in vars
        push!(lines,
              "tiramisu::computation $(var.computation.name)(" *
              "\"" * createISL(var.computation) * "\", " *
              createTExpr(var.computation.expr) * ", " *
              string(!var.computation.is_input) * ", " *
              jToT[var.computation.typ] * ", " *
              "&$function_name);")
    end
    for (id, var) in vars
        push!(lines,
              "tiramisu::buffer $(var.buffer.name)(" *
              "\"$(var.buffer.name)\", " *
              string(length(var.buffer.dimensions)) * ", " *
              "{" * join(map(createTExpr, var.buffer.dimensions), ", ") * "}, " *
              jToT[var.buffer.typ] * ", " *
              "NULL, " *
              argNames[var.buffer.argument] * ", ",
              "&$function_name);")
        push!(lines,
              "$(var.computation.name).set_access(\"{$(var.computation.name)[" *
                getIndices(var.computation) * "] -> " *
                "$(var.buffer.name)[" *
                getIndices(var.computation) * "]}\");")
    end
    interface = [var.buffer.name for (id, var) in vars if var.buffer.argument != TBTemporary]
    push!(lines, "$function_name.set_arguments({" *
                  join(map(s -> "&" * s, interface), ", ")
                  *"});")
    push!(lines, "$function_name.gen_time_space_domain();")
    push!(lines, "$function_name.gen_isl_ast();")
    push!(lines, "$function_name.gen_halide_stmt();")
    push!(lines, "$function_name.gen_halide_obj(\"$OBJ\");")
    push!(lines, "return 0;")
    return join(lines, '\n')
end

function buffer_to_arg_prototype(buffer)
    return (jToC[buffer.typ] *
            repeat("*", length(buffer.dimensions)) *
            " $(buffer.name)")
end

function buffer_to_halide_buffer(buffer)
    return ("Halide::Buffer<$(jToC[buffer.typ])> " *
            "$(buffer.name)_halide($(buffer.name), " *
            join(map(d -> "$(d.val)", buffer.dimensions), ", ") *
            ");")
end

function buffer_to_raw_buffer(buffer)
    return "$(buffer.name)_halide.raw_buffer()"
end

function generate_wrapper()
    # computation pass
    lines = Vector{String}()
    push!(lines, "#include \"Halide.h\"")
    push!(lines, "#include <tiramisu/utils.h>")
    push!(lines, "#include <cstdlib>")
    push!(lines, "")
    push!(lines, "int main(int, char**) {")
    push!(lines, "return 0;")
    push!(lines, "}")
    interface = [var.buffer for (id, var) in vars if var.buffer.argument != TBTemporary]
    push!(lines, "extern \"C\" void $(function_name)_wrapper(" *
                  join(map(buffer_to_arg_prototype, interface), ", ") *
                  ") {")
    for buffer in interface
        push!(lines, buffer_to_halide_buffer(buffer))
    end
    push!(lines, "$function_name(" *
                  join(map(buffer_to_raw_buffer, interface), ", ") *
                  ");")
    push!(lines, "}")
    return join(lines, '\n')
end



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
    push!(s,"-I$ISL_INCLUDE_DIR")
    push!(s,"-I/usr/local/include","-I$HALIDE_INCLUDE_DIR")
    push!(s,"-I$HALIDE_TOOLS_DIR","-I$TIRAMISU_BUILD_DIR")
    push!(s,"-L$ISL_LIB_DIR")
    push!(s,"-L/usr/local/lib","-lisl","-lgmp","-L$HALIDE_LIB_DIR","-lHalide","-ldl")
    push!(s,"-lpthread","-lz","-ljpeg","-ltinfo")#`libpng-config --cflags --ldflags`","-ljpeg")
    Cmd(s)
end

#Returns the command to run the function generator that was compiled by the Tiramisu file
function tiramisu_get_gen()
    env = copy(ENV)
    if haskey(env, "LD_LIBRARY_PATH") && env["LD_LIBRARY_PATH"] != ""
        env["LD_LIBRARY_PATH"]*= ":$HALIDE_INCLUDE_DIR/../lib:/usr/local/lib:$TIRAMISU_BUILD_DIR:$ISL_LIB_DIR"
    else
        env["LD_LIBRARY_PATH"] = "$HALIDE_INCLUDE_DIR/../lib:/usr/local/lib:$TIRAMISU_BUILD_DIR:$ISL_LIB_DIR"
    end
    if haskey(env, "DYLD_LIBRARY_PATH") && env["DYLD_LIBRARY_PATH"] != ""
        env["DYLD_LIBRARY_PATH"]*= ":$HALIDE_INCLUDE_DIR/../lib:$TIRAMISU_BUILD_DIR:$ISL_LIB_DIR"
    else
        env["DYLD_LIBRARY_PATH"] = "$HALIDE_INCLUDE_DIR/../lib:$TIRAMISU_BUILD_DIR:$ISL_LIB_DIR"
    end
    s = Cmd(["$GEN"])
    setenv(s,env)
end

#Returns the command to convert the generated function from an object file to a
#dynamic shared library file. This is necessary because Julia could only use c++
#code stored in libraries as opposed to object files
function tiramisu_get_lib_obj()
    s = ["g++","-c","-std=c++11","-O3","-Wall","-Wno-sign-compare","-fno-rtti","-fvisibility=hidden","-fPIC"]
    push!(s,"-I$TIRAMISU_INCLUDE_DIR")
    push!(s,"-I$ISL_INCLUDE_DIR")
    push!(s,"-I/usr/local/include","-I$HALIDE_INCLUDE_DIR")
    push!(s,"-I$HALIDE_TOOLS_DIR","-I$TIRAMISU_BUILD_DIR")
    push!(s,"-L$ISL_LIB_DIR")
    push!(s,"-L/usr/local/lib","-lisl","-lgmp","-L$HALIDE_LIB_DIR","-lHalide","-ldl")
    push!(s,"-lpthread","-lz","-ljpeg","-ltinfo")#`libpng-config --cflags --ldflags`","-ljpeg")
    push!(s,"-shared","-fPIC","-o","$OBJ_WRAPPER")
    Cmd(s)
end

#Returns the command to convert the generated function from an object file to a
#dynamic shared library file. This is necessary because Julia could only use c++
#code stored in libraries as opposed to object files
function tiramisu_get_lib()
    s = ["g++","-g","-std=c++11","-O3","-Wall","-Wno-sign-compare","-fno-rtti","-fvisibility=hidden","-fPIC"]
    push!(s,"$TIRAMISU_BUILD_DIR/tiramisu_core.o")
    push!(s,"$TIRAMISU_BUILD_DIR/tiramisu_codegen_halide.o")
    push!(s,"$TIRAMISU_BUILD_DIR/tiramisu_codegen_c.o")
    push!(s,"$TIRAMISU_BUILD_DIR/tiramisu_debug.o")
    push!(s,"$TIRAMISU_BUILD_DIR/tiramisu_utils.o")
    push!(s,"$TIRAMISU_BUILD_DIR/tiramisu_codegen_halide_lowering.o")
    push!(s,"$TIRAMISU_BUILD_DIR/tiramisu_codegen_from_halide.o")
    push!(s,OBJ)
    push!(s,OBJ_WRAPPER)
    push!(s,"-I$TIRAMISU_INCLUDE_DIR")
    push!(s,"-I$ISL_INCLUDE_DIR")
    push!(s,"-I/usr/local/include","-I$HALIDE_INCLUDE_DIR")
    push!(s,"-I$HALIDE_TOOLS_DIR","-I$TIRAMISU_BUILD_DIR")
    push!(s,"-L$ISL_LIB_DIR")
    push!(s,"-L/usr/local/lib","-lisl","-lgmp","-L$HALIDE_LIB_DIR","-lHalide","-ldl")
    push!(s,"-lpthread","-lz","-ljpeg","-ltinfo")#`libpng-config --cflags --ldflags`","-ljpeg")
    push!(s,"-shared","-fPIC","-o","$LIB")
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

end
