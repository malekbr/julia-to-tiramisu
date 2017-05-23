Julia To Tiramisu
===================

The goal of this project is to be able to convert Julia code into the Tiramisu framework, to be able to generate optimized Julia programs. To accomplish this task, we are building off an existing Julia package called ParallelAccelerator.jl. By using ParallelAccelerator.jl, this project adds an optimization pass that converts a Julia AST to Tiramisu generated code, effectively replacing PA's our C++ generation pass. For more information on ParallelAccelerator itself, check out this [blog post](https://julialang.org/blog/2016/03/parallelaccelerator) or peruse through the [git repo](https://github.com/IntelLabs/ParallelAccelerator.jl). The steps below all assume that both Tiramisu and Julia are both installed and built on your machine, but if not you could look how to do so [here for Tiramisu](https://github.com/rbaghdadi/tiramisu) and [here for Julia](https://julialang.org/downloads).

Install ParallelAccelerator Package
----------
First, you're going to want to get a copy of the current repo (with Tiramisu additions) or get access to someone who does (probably Riyadh). After having access to the repository URL, open the `url` file within the `METADATA` directory for ParallelAccelerator (in Linux, this could be found in `~/.julia/METADATA/ParrallelAccelerator/url`). This is a simple `.txt` file with a single line, which we will replace with the repository repo containing the Tiramisu transformations. Save and exit the file.

After making this change, open the terminal and type `julia` enter to open the Julia REPL. Now simply just type in `Pkg.add("ParallelAccelerator")` and this will add the package to Julia using the url we substituted above. Now that the package is installed, we could first check that running a ParallelAccelerator example works before trying a Tiramisu example.

Running Black-Scholes Example
-----------
In the terminal (quit out of the Julia REPL if you haven't already by running `quit()`), `cd` into the directory containing the ParallelAccelerator repository (in Linux this was `/.julia/v0.5/ParallelAccelerator`). This is the root of the directory. From here there are two subdirectories that we are primarily concerned with which are `src/` and `examples/`. To make sure ParallelAccelerator was added successfully run `julia examples/black-scholes/black-scholes.jl`.

Running Tiramisu Example
------------
Before running a Tiramisu example, we first must edit a couple of constants. Open `src/cgen-tiramisu.jl`. This is the file that contains a majority of the Julia to Tiramisu transformation project so far.  Before delving into the specifics of the file, scroll to around line 63 and edit the values of the TODO's to match the respective file location for each constant. It might also be a good idea to check the next set of constants are consistent with where you as a user want the files to be generated. After saving those edits one could go back to the terminal and run `TIRAMISU_MODE=tiramisu julia examples/tiramisu/mult.jl` to make sure the basic matrix multiplication example is working. The `TIRAMISU_MODE=tiramisu` command prefix is an environment variable that instructs ParallelAccelerator to use Tiramisu instead of its previously available C++ generator. Additionally, within the `examples/tiramisu/` directory, one could find other examples of programs that have been worked on, but with the exception of `hello.jl` are not fully supported yet.

How It Works
--------------
The three main files to look at, in order of increasing exponential importance, are `src/ParallelAccelerator.jl`, `src/driver.jl`, and `src/cgen-tiramisu.jl`. 

The first file `src/ParallelAccelerator.jl` specifies in its `__init__` method all of the optimization passes it will pass the Julia AST through. The only major modifications made in this files are those around the `TIRAMISU_MODE` environment variable, which when set to `tiramisu` tells ParallelAccelerator to use the `toTiramisu` optimization pass instead of `toCGen`. All of these optimization passes are defined in `driver.jl`.

In the second file, `src/driver.jl`, is where the `toTiramisu` optimization pass is defined as we previously mentioned. Each optimization pass calls an entry point method of its own modules and so `toTiramisu` was designed the same way, calling from the `Tiramisu` module defined in `src/cgen-tiramisu.jl`. After generating the function, `toTiramisu` creates a proxy function to return to the original Julia program that just uses Julia's `ccall` method which could call any C++ function stored in a shared dynamic library (and for our purposes that is the generated function by Tiramisu).  

The final file, `cgen-tiramisu.jl` is where the majority of the work happens in the project. Most of the description of how this transformation happens is documented on the file itself, but at a high level the program first converts the Julia AST to an array of Tiramisu Data Type Nodes, and then second translates these nodes into each individual C++ code. 
