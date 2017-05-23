using ParallelAccelerator

function hello()
    iterations = 100
    A = zeros(iterations)
    #initiiialize to value
    A = map(x->2,A)
    A = map(x->x+1,A)
    #end
    return A
end

function main()
    tic()
    B = hello()
    println("SELFTIMED ", toq())

    println(B[end])

end

main()