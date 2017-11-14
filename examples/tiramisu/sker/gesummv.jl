using ParallelAccelerator

@acc function kernel_gesummv(n::Int64, alpha::Float64, beta::Float64,
                             A::Matrix{Float64}, B::Matrix{Float64},
                             x::Vector{Float64})
    y = Array{Float64}(n)
    for i = 1:n
        t = 0.0
        y[i] = 0.0
        for j = 1:n
            t = A[i, j] * x[j] + t
            y[i] = B[i, j] * x[j] + y[i]
        end
        y[i] = alpha * t + beta * y[i]
    end
    return y
 end

@inbounds function kernel_gesummv_o(n::Int64, alpha::Float64, beta::Float64,
                             A::Matrix{Float64}, B::Matrix{Float64},
                             x::Vector{Float64})
    tic()
    y = Array{Float64}(n)
    for i = 1:n
        t = 0.0
        y[i] = 0.0
        for j = 1:n
            t = A[i, j] * x[j] + t
            y[i] = B[i, j] * x[j] + y[i]
        end
        y[i] = alpha * t + beta * y[i]
    end
    println("Julia ", toq())
    return y
 end

 n = 15000
 alpha = 1.5
 beta = 1.2

 A = rand(n, n)
 B = rand(n, n)
 x = rand(n)

res_tiramisu = kernel_gesummv(n, alpha, beta, A, B, x)
res_julia = kernel_gesummv_o(n, alpha, beta, A, B, x)


function compare(A, B)
    for i in 1:n
        if A[i] != B[i]
            println("different $((i)) : $(A[i]) != $(B[i])")
            return false
        end
    end
    return true
end

println(compare(res_tiramisu, res_julia))
