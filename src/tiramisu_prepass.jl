module TiramisuPrepass
export detector

# Return the first match
type AnyOf
  matchers::Function
end

Matcher = Union{Function, AnyOf}

# A matcher takes two variables:
# - An expression::Expr
# - The index of the expression in the top level of the AST
# - A state (doesn't matter in the first matcher)
# It returns:
# - A boolean saying whether there is a match
# - A state to use for the next matcher, or validator
# A validator is called with the state returned from the last matcher
# when a pattern is accepted
type Pattern
  matchers::Vector{Matcher}
  validator::Function
end

abstract State

type NoState <: State
end

type ForLoopHeaderSLE <: State
  lower::Integer
  upper::Integer
  result::SSAValue
end


function isForLoopHeaderSLE(expr::Expr, index::Integer, state::State)
  is_valid = (expr.head == :(=) &&
              typeof(expr.args[2]) <: Expr &&
              expr.args[2].head == :call &&
              expr.args[2].args[1] == GlobalRef(Core.Intrinsics, :sle_int))
  if is_valid
    return true, ForLoopHeaderSLE(expr.args[2].args[2],
                                  expr.args[2].args[3],
                                  expr.args[1])
  end
  return false, NoState()
end

type ForLoopHeaderSubInt <: State
  lower::Integer
  upper::Integer
  sle_result::SSAValue
  sub_int_result::SSAValue
end

function isForLoopHeaderSubInt(expr::Expr, index::Integer, state::State)
  is_valid = (expr.head == :(=) &&
              typeof(expr.args[2]) <: Expr &&
              expr.args[2].head == :call &&
              expr.args[2].args[1] == GlobalRef(Core.Intrinsics, :box) &&
              typeof(expr.args[2].args[3]) <: Expr &&
              expr.args[2].args[3].head == :call &&
              expr.args[2].args[3].args[1] == GlobalRef(Core.Intrinsics, :sub_int) &&
              expr.args[2].args[3].args[2] == state.lower &&
              expr.args[2].args[3].args[3] == 1)

  if is_valid
    return true, ForLoopHeaderSubInt(state.lower,
                                     state.upper,
                                     state.result,
                                     expr.args[1])
  end
  return false, NoState()
end

type ForLoopHeaderSelect <: State
  lower::Integer
  upper::Integer
  result::SSAValue
end

function isForLoopHeaderSelect(expr::Expr, index::Integer, state::State)
  is_valid = (expr.head == :(=) &&
              typeof(expr.args[2]) <: Expr &&
              expr.args[2].head == :call &&
              expr.args[2].args[1] == GlobalRef(Core.Intrinsics, :select_value) &&
              expr.args[2].args[2] == state.sle_result &&
              expr.args[2].args[3] == state.upper &&
              expr.args[2].args[4] == state.sub_int_result)

  if is_valid
    return true, ForLoopHeaderSelect(state.lower,
                                     state.upper,
                                     expr.args[1])
  end
  return false, NoState()
end

type ForLoopHeaderAddInt <: State
  lower::Integer
  upper::Integer
  result::SSAValue
end

function isForLoopHeaderAddInt(expr::Expr, index::Integer, state::State)
  is_valid = (expr.head == :(=) &&
              typeof(expr.args[2]) <: Expr &&
              expr.args[2].head == :call &&
              expr.args[2].args[1] == GlobalRef(Core.Intrinsics, :box) &&
              typeof(expr.args[2].args[3]) <: Expr &&
              expr.args[2].args[3].head == :call &&
              expr.args[2].args[3].args[1] == GlobalRef(Core.Intrinsics, :add_int) &&
              expr.args[2].args[3].args[2] == state.result &&
              expr.args[2].args[3].args[3] == 1)

  if is_valid
    return true, ForLoopHeaderAddInt(state.lower,
                                     state.upper,
                                     expr.args[1])
  end
  return false, NoState()
end

function forLoopHeaderValidator(state::ForLoopHeaderAddInt)
  println("Detected for loop from $(state.lower) to $(state.upper) at $(state.result)")
end


#### Basic detector

for_loop_header_pattern = Pattern([isForLoopHeaderSLE,
                                   isForLoopHeaderSubInt,
                                   isForLoopHeaderSelect,
                                   isForLoopHeaderAddInt],
                                  forLoopHeaderValidator)

patterns = [for_loop_header_pattern]

function detector(ast::Expr)
  states = State[NoState() for state in ast.args]
  indices = [1 for expr in ast.args]
  for (e_i, expr) in enumerate(ast.args)
    # TODO: fix
    if !(typeof(expr) <: Expr)
      continue
    end
    for (p_i, pattern) in enumerate(patterns)
      matched, state = pattern.matchers[indices[p_i]](expr, e_i, states[p_i])
      if matched
        if indices[p_i] == length(pattern.matchers)
          # pattern matched!
          pattern.validator(state)
          states = State[NoState() for expr in ast.args]
          indices = [1 for expr in ast.args]
          break
        else
          # pattern not matched yet
          indices[p_i] += 1
          states[p_i] = state
        end
      else
        indices[p_i] = 1
        states[p_i] = NoState()
      end
    end
  end
end

end
