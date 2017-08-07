module TiramisuPrepass
export detector

SupportedNode = Union{LabelNode, GotoNode, Expr, NewvarNode, LineNumberNode}

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

type Changer
  ignore::Set{Integer}
  changers::Dict{Integer, Function}
end

abstract State

type NoState <: State
end

function onlyExpr(f::Function)
  return function(node::SupportedNode, index::Integer, state::State)
    if !(typeof(node) <: Expr)
      return false, NoState()
    end
    return f(node, index, state)
  end
end

function onlyLabelNode(f::Function)
  return function(node::SupportedNode, index::Integer, state::State)
    if !(typeof(node) <: LabelNode)
      return false, NoState()
    end
    return f(node, index, state)
  end
end

function onlyExprB(f::Function)
  return function(node::SupportedNode, index::Integer, state::State)
    if !(typeof(node) <: Expr)
      return false
    end
    return f(node, index, state)
  end
end

function onlyLabelNodeB(f::Function)
  return function(node::SupportedNode, index::Integer, state::State)
    if !(typeof(node) <: LabelNode)
      return false
    end
    return f(node, index, state)
  end
end

function onlyGotoNodeB(f::Function)
  return function(node::SupportedNode, index::Integer, state::State)
    if !(typeof(node) <: GotoNode)
      return false
    end
    return f(node, index, state)
  end
end
# FOR LOOP HEADER DETECTION

type ForLoopHeaderSLE <: State
  lower::Integer
  upper::Integer
  top_index::Integer
  result::SSAValue
end

function isForLoopHeaderSLE(expr::Expr, index::Integer, state::NoState)
  is_valid = (expr.head == :(=) &&
              typeof(expr.args[2]) <: Expr &&
              expr.args[2].head == :call &&
              expr.args[2].args[1] == GlobalRef(Core.Intrinsics, :sle_int))
  if is_valid
    return true, ForLoopHeaderSLE(expr.args[2].args[2],
                                  expr.args[2].args[3],
                                  index,
                                  expr.args[1])
  end
  return false, NoState()
end

type ForLoopHeaderSubInt <: State
  lower::Integer
  upper::Integer
  top_index::Integer
  sle_result::SSAValue
  sub_int_result::SSAValue
end

function isForLoopHeaderSubInt(expr::Expr, index::Integer, state::ForLoopHeaderSLE)
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
                                     state.top_index,
                                     state.result,
                                     expr.args[1])
  end
  return false, NoState()
end

type ForLoopHeaderSelect <: State
  lower::Integer
  upper::Integer
  top_index::Integer
  result::SSAValue
end

function isForLoopHeaderSelect(expr::Expr, index::Integer, state::ForLoopHeaderSubInt)
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
                                     state.top_index,
                                     expr.args[1])
  end
  return false, NoState()
end

type ForLoopHeaderAddInt <: State
  lower::Integer
  upper::Integer
  top_index::Integer
  result::SSAValue
end

function isForLoopHeaderAddInt(expr::Expr, index::Integer, state::ForLoopHeaderSelect)
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
                                     state.top_index,
                                     expr.args[1])
  end
  return false, NoState()
end

type ForLoopData <: State
  lower::Integer # lower bound of loop
  upper::Integer # upper bound of loop
  protec_upper::SSAValue # Computed upper bound of loop
  loop_var::Union{Void, SlotNumber} # Actual loop variable
  used_loop_var::Union{Void, SlotNumber} # Exposed loop variable (if variable is used)
  begin_loop_label::Union{Void, Int64} # Label for reloop
  cont_loop_label::Union{Void, Int64} # Uncertain? There is some label before last Goto
  end_loop_label::Union{Void, Int64} # Label to end loop
  comp_res::Union{Void, SSAValue} # result of loop_var === protec_upper
  cond_res::Union{Void, SSAValue} # result of not(comp_res)
  loop_var_temp::Union{Void, SSAValue} # In case of used_loop_var, original value stored here
  top_index::Integer # Index of loop header
  start_begin_index::Integer # Starting index of loop start
  start_end_index::Integer # Ending index of loop start
  bottom_index::Integer # Index of Goto at loop end
end

for_loop_changer = Changer(Set(1:0), Dict{Integer, Function}())

function forLoopHeaderValidator(state::ForLoopHeaderAddInt, ast::Expr)
  println("Detected for loop from $(state.lower) to $(state.upper) at $(state.result)")
  finalState = ForLoopData(state.lower, state.upper, state.result,
                            nothing, nothing, nothing, nothing, nothing,
                            nothing, nothing, nothing, state.top_index,
                            0, 0, 0)
  top_found = false
  for (i, expr) in enumerate(ast.args)
    # order is important
    if top_found
      if (isForLoopEndGoto(expr, i,  finalState) &&
          isForLoopEndLabel(ast.args[i + 1], i + 1, finalState))
        for_loop_changer.ignore = Set(union(for_loop_changer.ignore,
                   finalState.top_index:(finalState.top_index + 3)))
        println(for_loop_changer.ignore)
        for_loop_changer.ignore = Set(union(for_loop_changer.ignore,
                   (finalState.start_begin_index + 1):finalState.start_end_index))
        println(for_loop_changer.ignore)
        for_loop_changer.ignore = Set(union(for_loop_changer.ignore,
                   (finalState.bottom_index + 1):(finalState.bottom_index + 1)))
        println(for_loop_changer.ignore)
        for_loop_changer.changers[finalState.start_begin_index] = function()
          return Expr(:for_loop_start, finalState.protec_upper,
                      finalState.lower, finalState.upper, 1,
                      finalState.loop_var, finalState.used_loop_var)
        end
        for_loop_changer.changers[finalState.bottom_index] = function()
          return Expr(:for_loop_end, finalState.protec_upper)
        end
        println("Done")
        break
      end
    else
      if (isForLoopTestComp(expr, i, finalState) &&
          isForLoopTestInit(ast.args[i - 2], i - 2, finalState) &&
          isForLoopTestLabel(ast.args[i - 1], i - 1, finalState) &&
          isForLoopTestCond(ast.args[i + 1], i + 1, finalState) &&
          isForLoopTestGoto(ast.args[i + 2], i + 2, finalState) &&
          ((isForLoopIncUsedAdd(ast.args[i + 3], i + 3, finalState) &&
            isForLoopIncUsedVar(ast.args[i + 4], i + 4, finalState) &&
            isForLoopIncUsedUpdate(ast.args[i + 5], i + 5, finalState)) ||
           isForLoopIncUnusedAdd(ast.args[i + 3], i + 3, finalState)))
        top_found = true
      end
    end
  end
end


# FOR LOOP TEST DETECTION

# Order: Comp -> Init -> Label -> Cond -> Goto

isForLoopTestInit = onlyExprB(function(expr::Expr, index::Integer, state::ForLoopData)
  is_valid = (expr.head == :(=) &&
              expr.args[1] == state.loop_var  &&
              expr.args[2] == state.lower)
  if is_valid
    state.start_begin_index = index
  end
  return is_valid
end)

isForLoopTestLabel = onlyLabelNodeB(function(label::LabelNode, index::Integer, state::ForLoopData)
  state.begin_loop_label = label.label
  return true
end)

isForLoopTestComp = onlyExprB(function(expr::Expr, index::Integer, state::ForLoopData)
  is_valid = (expr.head == :(=) &&
              typeof(expr.args[1]) <: SSAValue &&
              typeof(expr.args[2]) <: Expr &&
              expr.args[2].head == :call &&
              expr.args[2].args[1] == GlobalRef(Core, :(===)) &&
              typeof(expr.args[2].args[2]) <: SlotNumber &&
              expr.args[2].args[3] == state.protec_upper)

  if is_valid
    state.comp_res = expr.args[1]
    state.loop_var = expr.args[2].args[2]
  end

  return is_valid
end)

isForLoopTestCond = onlyExprB(function(expr::Expr, index::Integer, state::ForLoopData)
  is_valid = (expr.head == :(=) &&
              typeof(expr.args[1]) <: SSAValue &&
              typeof(expr.args[2]) <: Expr &&
              expr.args[2].head == :call &&
              expr.args[2].args[1] == GlobalRef(Core.Intrinsics, :box) &&
              typeof(expr.args[2].args[3]) <: Expr &&
              expr.args[2].args[3].head == :call &&
              expr.args[2].args[3].args[1] == GlobalRef(Core.Intrinsics, :not_int) &&
              expr.args[2].args[3].args[2] == state.comp_res)

  if is_valid
    state.cond_res = expr.args[1]
  end
  return is_valid
end)

isForLoopTestGoto = onlyExprB(function(expr::Expr, index::Integer, state::ForLoopData)
  is_valid = (expr.head == :gotoifnot &&
              expr.args[1] == state.cond_res &&
              typeof(expr.args[2]) <: Integer) # TODO always???????

  if is_valid
    state.end_loop_label = expr.args[2]
  end
  return is_valid
end)

isForLoopIncUsedAdd = onlyExprB(function(expr::Expr, index::Integer, state::ForLoopData)
  is_valid = (expr.head == :(=) &&
              typeof(expr.args[1]) <: SSAValue &&
              typeof(expr.args[2]) <: Expr &&
              expr.args[2].head == :call &&
              expr.args[2].args[1] == GlobalRef(Core.Intrinsics, :box) &&
              typeof(expr.args[2].args[3]) <: Expr &&
              expr.args[2].args[3].head == :call &&
              expr.args[2].args[3].args[1] == GlobalRef(Core.Intrinsics, :add_int) &&
              expr.args[2].args[3].args[2] == state.loop_var &&
              expr.args[2].args[3].args[3] == 1)

  if is_valid
    state.loop_var_temp = expr.args[1]
  end
  return is_valid
end)

isForLoopIncUsedVar = onlyExprB(function(expr::Expr, index::Integer, state::ForLoopData)
  is_valid = (expr.head == :(=) &&
              typeof(expr.args[1]) <: SlotNumber &&
              expr.args[2] == state.loop_var)

  if is_valid
    state.used_loop_var = expr.args[1]
  end
  return is_valid
end)


isForLoopIncUsedUpdate = onlyExprB(function(expr::Expr, index::Integer, state::ForLoopData)
  is_valid = (expr.head == :(=) &&
              expr.args[1] == state.loop_var &&
              expr.args[2] == state.loop_var_temp)
  if is_valid
    state.start_end_index = index
  end
  return is_valid
end)
isForLoopIncUnusedAdd = onlyExprB(function(expr::Expr, index::Integer, state::ForLoopData)
  is_valid = (expr.head == :(=) &&
              expr.args[1] == state.loop_var &&
              typeof(expr.args[2]) <: Expr &&
              expr.args[2].head == :call &&
              expr.args[2].args[1] == GlobalRef(Core.Intrinsics, :box) &&
              typeof(expr.args[2].args[3]) <: Expr &&
              expr.args[2].args[3].head == :call &&
              expr.args[2].args[3].args[1] == GlobalRef(Core.Intrinsics, :add_int) &&
              expr.args[2].args[3].args[2] == state.loop_var &&
              expr.args[2].args[3].args[3] == 1)
  if is_valid
    state.start_end_index = index
  end
  return is_valid
end)

isForLoopEndGoto = onlyGotoNodeB(function(expr::GotoNode, index::Integer, state::ForLoopData)
  is_valid = expr.label == state.begin_loop_label
  if is_valid
    state.bottom_index = index
  end
  return is_valid
end)

isForLoopEndLabel = onlyLabelNodeB(function(expr::LabelNode, index::Integer, state::ForLoopData)
  state.end_loop_label = expr.label
  return true
end)
#### Basic detector

for_loop_header_pattern = Pattern([onlyExpr(isForLoopHeaderSLE),
                                   onlyExpr(isForLoopHeaderSubInt),
                                   onlyExpr(isForLoopHeaderSelect),
                                   onlyExpr(isForLoopHeaderAddInt)],
                                  forLoopHeaderValidator)

for_loop_patterns = [for_loop_header_pattern] #, for_loop_test_pattern]

function detector(ast::Expr)
  remove_unused(ast)
  detector(ast, for_loop_patterns)
  # dump(change(ast, [for_loop_changer]))
end

first_else(iter, e) = isempty(iter) ? e : first(iter)

function change(ast::Expr, changers::Vector{Changer})
  # I heard you like functional programming, so...
  return [first_else((c.changers for c in changers if haskey(c.changers, i)),
                         Dict(i => () -> e))[i]()
          for (i, e) in enumerate(ast.args)
          if !any(i in c.ignore for c in changers)]
end

function detector(ast::Expr, patterns::Vector{Pattern})
  states = State[NoState() for state in patterns]
  indices = [1 for expr in patterns]
  for (e_i, expr) in enumerate(ast.args)
    for (p_i, pattern) in enumerate(patterns)
      @label pattern_check
      matched, state = pattern.matchers[indices[p_i]](expr, e_i, states[p_i])
      if matched
        if indices[p_i] == length(pattern.matchers)
          # pattern matched!
          pattern.validator(state, ast)
          states = State[NoState() for expr in ast.args]
          indices = [1 for expr in ast.args]
          break
        else
          # pattern not matched yet
          indices[p_i] += 1
          states[p_i] = state
        end
      else
        restart_pattern = indices[p_i] > 1
        indices[p_i] = 1
        states[p_i] = NoState()
        if restart_pattern
          @goto pattern_check # TODO: gotos are evil and need to be stopped
        end
      end
    end
  end
end

Variable = Union{Slot, SSAValue}

function get_used_def(expr::Expr)
  # TODO support side effects, and nested assignments
  def = nothing
  if expr.head == :(=)
    def = expr.args[1]
  end
  used = union((u for (u, d) in map(get_used_def, expr.args))...)
  delete!(used, def)
  return used, Set{Variable}(def == nothing ? [] : [def])
end

function get_used_def(expr::Variable)
  return Set{Variable}([expr]), Set{Variable}()
end

function get_used_def(expr::Any)
  return Set{Variable}(), Set{Variable}()
end

function remove_unused(ast::Expr)
  # println("Used, def")
  cleaned = false
  while !cleaned
    used = Set{Variable}()
    def = Set{Variable}()
    for expr in ast.args
      u, d = get_used_def(expr)
      union!(used, u)
      union!(def, d)
    end
    unused = setdiff(def, used)
    # println(unused)
    ast.args = [e for e in ast.args
                if !(isa(e, Expr) && e.head == :(=) && e.args[1] in unused)]
    cleaned = length(unused) == 0
  end
end
end
