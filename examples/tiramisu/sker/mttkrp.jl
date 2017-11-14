using ParallelAccelerator

@acc function mttkrp(I::Int, J::Int,
                     K::Int, L::Int,
                     B::Array{Float64, 3}, C::Matrix{Float64},
                     D::Matrix{Float64})

    A = Matrix{Float64}(I, J)
    for i = 1:I
        for j = 1:J
            s = 0.0
            for k = 1:K
                t = 0.0
                for l = 1:L
                    t = t + B[i, k, l] * D[l, j]
                end
                s = s + t * C[k, j]
            end
            A[i, j] = s
        end
    end
    return A
end

I, J, K, L = (200, 200, 200, 200)

B = randn(I, K, L)
C = randn(K, J)
D = randn(L, J)


res1 = mttkrp(I, J, K, L, B, C, D)

@inbounds function mttkrp_o(I::Int, J::Int,
                     K::Int, L::Int,
                     B::Array{Float64, 3}, C::Matrix{Float64},
                     D::Matrix{Float64})

    tic()
    A = Matrix{Float64}(I, J)
    for i = 1:I
        for j = 1:J
            s = 0.0
            for k = 1:K
                t = 0.0
                for l = 1:L
                    t = t + B[i, k, l] * D[l, j]
                end
                s = s + t * C[k, j]
            end
            A[i, j] = s
        end
    end
    println("Julia ", toq())
    return A
 end

res2 = mttkrp_o(I, J, K, L, B, C, D)

for i = 1:I
    exit = false
    for j = 1:J
        if res2[i, j] != res1[i, j]
            println("different $(res2[i, j]) $(res1[i,j]) $(res1[j, i])")
            exit = true
            break
        end
    end
    if exit
        break
    end
end

function compare(A, B)
    for i in 1:I
        for j in 1:J
            if A[i, j] != B[i, j]
                println("different $((i, j)) : $(A[i, j]) != $(B[i, j])")
                return false
            end
        end
    end
    return true
end

println(compare(res1, res2))
