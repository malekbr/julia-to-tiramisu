using ParallelAccelerator
using DocOpt

@acc function hello(iterations)
    A = Array{Int64}(iterations)
    for i in 1:iterations
      A[i] = 0
    end
    return A
end

function main()
    doc = """hello.jl

Tiramisu Hello World Example

Usage:
  hello.jl -h | --help
  hello.jl [--iterations=<iterations>]

Options:
  -h --help                  Show this screen.
  --iterations=<iterations>  Specify a number of iterations [default: 100].
"""
    arguments = docopt(doc)
    iterations = parse(Int, arguments["--iterations"])

    println("iterations = ", iterations)
    println("output file = ", out_file)

    tic()
    hello(1)
    println("SELFPRIMED ", toq())

    tic()
    B = hello(iterations)
    println("SELFTIMED ", toq())

end

main()