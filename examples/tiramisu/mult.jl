using ParallelAccelerator

@acc function mult()
    w = 5#1000
    h = 5#1000
    A = fill(1,h,w)
    B = fill(1,h,w)
    C = fill(0,h,w)
    C += A*B
    return C
end

function main()
    tic()
    D = mult()
    println("SELFTIMED ", toq())

    #println(D)

end

main()