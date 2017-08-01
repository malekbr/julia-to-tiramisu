using ParallelAccelerator

@acc function add()
  # w = 5
  # h = 5
  # A = fill(1, w, h)
  # B = fill(1, w, h)
  # C = A + B
  a = 1 + 2
  b = 3 + 4 +5
  return 1 + 2 + a + b
end

function main()
    tic()
    D = add()
    println("SELFTIMED ", toq())

    println(D)

end

main()
