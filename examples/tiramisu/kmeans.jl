using ParallelAccelerator

@acc function kmeans()
    numCenter = 5
    iterNum = 30
    points = rand(20,10000)
    D,N = size(points) # number of features, instances
    centroids = rand(D, numCenter)

    for l in 1:iterNum
        dist :: Array{Array{Float64,1},1} = [ Float64[sqrt(sum((points[:,i].-centroids[:,j]).^2)) for j in 1:numCenter] for i in 1:N]
        labels :: Array{Int, 1} = [indmin(dist[i]) for i in 1:N]
        centroids :: Array{Float64,2} = [ sum(points[j,labels.==i])/sum(labels.==i) for j in 1:D, i in 1:numCenter]
    end
    return centroids
end

function main()

    tic()
    centroids_out = kmeans()
    time = toq()
    println("result = ", centroids_out)
    println("SELFTIMED ", time)

    #benchmark()

end

function benchmark()
  maxt = 0
  mint = Inf
  totalt = 0
  for i in 1:100
    tic()
    centroids_out = kmeans()
    t = toq()
    println("finished ",i)
    if t > maxt
      maxt = t
    end
    if t < mint
      mint = t
    end
    totalt += t
  end
  println("SELFTIMED AVERAGE ", totalt/100.0)
  println("SELFTIMED MINIMUM", mint)
  println("SELFTIMED MAXIMUM", maxt)
end


main()
