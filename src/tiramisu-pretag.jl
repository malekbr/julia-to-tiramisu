module TiramisuPretag

using ..ParallelAccelerator

import CompilerTools

export process_node

type State
  forCount::Integer
  ifCount::Integer
  currentFor::Integer
end

function process_node(node::Expr, state::Union{State, Void}, top_level_number, is_top_level, read)
  if state === nothing
    state = State(0, 0, -1)
  end

  parentFor = nothing

  if node.head === :for
    parentFor = state.currentFor
    state.currentFor = state.forCount
    state.forCount += 1
  elseif node.head === :if
    state.ifCount += 1
  end

  children = Vector{Any}()
  for arg in node.args
    if isa(arg, Expr)
      if arg.head === :for
        push!(children, Expr(:meta, :forloop, state.forCount))
      elseif arg.head === :continue
        push!(children, Expr(:meta, :continue, state.currentFor))
      elseif arg.head === :if
        push!(children, Expr(:meta, :if, state.ifCount))
        if 3 <= length(arg.args)  # If there is an else
          index = 1
          elseblock = arg.args[3].args
          if isa(elseblock[1], Expr) && elseblock[1].head === :line
            index = 2 # skip line info
          end
          insert!(elseblock, index, Expr(:meta, :else, state.ifCount))
          push!(elseblock, Expr(:meta, :endif, state.ifCount))
        else
          push!(arg.args[2].args, Expr(:meta, :endif, state.ifCount))
        end
      end
    end
    push!(children, CompilerTools.AstWalker.AstWalk(arg, process_node, state))
  end

  node.args = children

  if node.head === :for
    state.currentFor = parentFor
  end

  return node
end

function process_node(node::Any, state::Union{State, Void}, top_level_number, is_top_level, read)
  CompilerTools.AstWalker.ASTWALK_RECURSE
end

end
