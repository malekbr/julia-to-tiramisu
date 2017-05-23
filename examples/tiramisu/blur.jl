using ParallelAccelerator

@acc function blurxy()
    w = 64
    h = 128
    inp = zeros(w,h)
    inp = map(i -> i[1],enumerate(inp))

    blur = map(i -> (inp[max(i[1]-1,1)]+inp[i[1]]+inp[min(i[1]+1,length(inp))])/3,enumerate(inp))
    return blur
end

function main()

    tic()
    blur_out = blurxy()
    time = toq()
    #println("result = ", blur_out)
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
