using ParallelAccelerator

@acc function covariance(m::Int, n::Int, float_n::Float64, data::Matrix{Float64})
    cov = Matrix{Float64}(m, m)
    mean = Vector{Float64}(m)
    data_m_mean = Matrix{Float64}(n, m)
    for j in 1:m
        mean[j] = 0.0
        for i = 1:n
            mean[j] += data[i, j]
        end
        mean[j] /= float_n
    end
    for i = 1:n
        for j = 1:m
            data_m_mean[i, j] = data[i, j] - mean[j]
        end
    end
    for i = 1:m
        for j = i:m
            cov[i, j] = 0.0
            for k = 1:n
                cov[i, j] += data_m_mean[k, i] * data_m_mean[k, j]
            end
            cov[i, j] /= (float_n - 1.0)
            cov[j, i] = cov[i, j]
        end
    end
    return cov
end

@inbounds function covariance_o(m::Int, n::Int, float_n::Float64, data::Matrix{Float64})
    cov = Matrix{Float64}(m, m)
    mean = Vector{Float64}(m)
    data_m_mean = Matrix{Float64}(n, m)
    tic()
    for j in 1:m
        mean[j] = 0.0
        for i = 1:n
            mean[j] += data[i, j]
        end
        mean[j] /= float_n
    end
    for i = 1:n
        for j = 1:m
            data_m_mean[i, j] = data[i, j] - mean[j]
        end
    end
    for i = 1:m
        for j = i:m
            cov[i, j] = 0.0
            for k = 1:n
                cov[i, j] += data_m_mean[k, i] * data_m_mean[k, j]
            end
            cov[i, j] /= (float_n - 1.0)
            cov[j, i] = cov[i, j]
        end
    end
    println(" Julia $(toq())")
    return cov
end

m = 2300
n = 2300
float_n = Float64(n)
data = Matrix{Float64}(n, m)
data = rand(n, m)

res_tiramisu = covariance(m, n, float_n, data)
res_julia  = covariance_o(m, n, float_n, data)


function compare(A, B)
    for i in 1:m
        for j in 1:m
            if A[i, j] != B[i, j]
                println("different $((i, j)) : $(A[i, j]) != $(B[i, j])")
                return false
            end
        end
    end
    return true
end

println(compare(res_tiramisu, res_julia))
