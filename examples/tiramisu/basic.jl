using ParallelAccelerator

@acc function add()
  # w = 5
  # h = 5
  # A = fill(1, w, h)
  # B = fill(1, w, h)
  # C = A + B
  #a = Matrix{Int64}(2, 4)
  #return a
  return 1
end

function main()
    tic()
    D = add()
    println("SELFTIMED ", toq())

    println(D)

end

main()
