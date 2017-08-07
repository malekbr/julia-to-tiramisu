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

type ForLoopData <: State
  from::Integer # lower bound of loop
  to::Integer # upper bound of loop
  protec_to::SSAValue # Computed upper bound of loop
  loop_var::Union{Void, SlotNumber} # Actual loop variable
  used_loop_var::Union{Void, SlotNumber} # Exposed loop variable (if variable is used)
  begin_label::Union{Void, Int64} # Label for reloop
  end_label::Union{Void, Int64} # Label to end loop
  temp_loop_var::Union{Void, SSAValue} # In case of used_loop_var, original value stored here
  inc_loop_var::Union{Void, SSAValue} # In case of used_loop_var, original value stored here
  header_index::Integer # Index of loop header
  top_index::Integer
  bottom_index::Integer # Index of Goto at loop end
end

isForLoopHeader = onlyExpr(function(expr::Expr, index::Integer, state::NoState)
  is_valid = (expr.head === :(=))

  if !is_valid
    return false, NoState()
  end

  sel_expr = expr.args[2]

  is_valid = (isa(sel_expr, Expr) &&
              sel_expr.head === :call &&
              sel_expr.args[1] === GlobalRef(Base, :select_value))

  if !is_valid
    return false, NoState()
  end

  comp_expr = sel_expr.args[2]

  is_valid &= (isa(comp_expr, Expr) &&
               comp_expr.head === :call &&
               comp_expr.args[1] === GlobalRef(Base, :sle_int))

  if !is_valid
    return false, NoState()
  end

  from = comp_expr.args[2]
  to = comp_expr.args[3]

  is_valid &= sel_expr.args[3] === to

  if !is_valid
    return false, NoState()
  end

  box_expr = sel_expr.args[4]

  is_valid &= (isa(box_expr, Expr) &&
               box_expr.head === :call &&
               box_expr.args[1] === GlobalRef(Base, :box) &&
               box_expr.args[2] === Int64)

  if !is_valid
    return false, NoState()
  end

  sub_expr = box_expr.args[3]

  is_valid &= (isa(sub_expr, Expr) &&
               sub_expr.head === :call &&
               sub_expr.args[1] === GlobalRef(Base, :sub_int) &&
               sub_expr.args[2] === from &&
               sub_expr.args[3] === 1)

  if is_valid
    state = ForLoopData(from, to, expr.args[1], nothing, nothing, nothing,
                        nothing, nothing, nothing, index, 0, 0)
    return true, state
  end
  return false, NoState()
end)

isForLoopVarInit = onlyExpr(function(expr::Expr, index::Integer, state::ForLoopData)
  is_valid = (expr.head === :(=) &&
              isa(expr.args[1], SlotNumber) &&
              expr.args[2] === state.from)
  state.loop_var = expr.args[1]
  return is_valid, state
end)

isForLoopBeginLabel = onlyLabelNode(function(label::LabelNode, index::Integer, state::ForLoopData)
  state.begin_label = label.label
  return true, state
end)

isForLoopTestGoto = onlyExpr(function(expr::Expr, index::Integer, state::ForLoopData)
  is_valid = expr.head === :gotoifnot

  if !is_valid
    return false, state
  end

  box_expr = expr.args[1]

  is_valid &= (isa(box_expr, Expr) &&
               box_expr.head === :call &&
               box_expr.args[1] === GlobalRef(Base, :box) &&
               box_expr.args[2] === GlobalRef(Base, :Bool))

  if !is_valid
    return false, state
  end

  not_expr = box_expr.args[3]

  is_valid &= (isa(not_expr, Expr) &&
               not_expr.head === :call &&
               not_expr.args[1] === GlobalRef(Base, :not_int))

  if !is_valid
    return false, state
  end

  comp_expr = not_expr.args[2]

  is_valid &= (isa(comp_expr, Expr) &&
               comp_expr.args[1] === GlobalRef(Base, :(===)) &&
               comp_expr.args[2] === state.loop_var)

  if !is_valid
    return false, state
  end

  box_expr = comp_expr.args[3]

  is_valid &= (isa(box_expr, Expr) &&
               box_expr.head === :call &&
               box_expr.args[1] === GlobalRef(Base, :box) &&
               box_expr.args[2] === Int64)

  if !is_valid
    return false, state
  end

  add_expr = box_expr.args[3]

  is_valid &= (isa(add_expr, Expr) &&
               add_expr.head === :call &&
               add_expr.args[1] === GlobalRef(Base, :add_int) &&
               add_expr.args[2] === state.protec_to &&
               add_expr.args[3] === 1)

  if is_valid
    state.end_label = expr.args[2]
  end

  return is_valid, state
end)

isForLoopVarSave = onlyExpr(function(expr::Expr, index::Integer, state::ForLoopData)
  is_valid = (expr.head === :(=) &&
              isa(expr.args[1], SSAValue) &&
              expr.args[2] === state.loop_var)
  if is_valid
    state.temp_loop_var = expr.args[1]
  end
  return is_valid, state
end)

isForLoopVarInc = onlyExpr(function(expr::Expr, index::Integer, state::ForLoopData)
  is_valid = (expr.head === :(=) &&
              isa(expr.args[1], SSAValue))

  if !is_valid
    return false, state
  end

  box_expr = expr.args[2]

  is_valid &= (isa(box_expr, Expr) &&
               box_expr.head === :call &&
               box_expr.args[1] === GlobalRef(Base, :box) &&
               box_expr.args[2] === Int64)

  if !is_valid
    return false, state
  end

  add_expr = box_expr.args[3]

  is_valid &= (isa(add_expr, Expr) &&
               add_expr.head === :call &&
               add_expr.args[1] === GlobalRef(Base, :add_int) &&
               add_expr.args[2] === state.loop_var &&
               add_expr.args[3] === 1)

  if is_valid
    state.inc_loop_var = expr.args[1]
  end

  return is_valid, state
end)

isForLoopUsedVar = onlyExpr(function(expr::Expr, index::Integer, state::ForLoopData)
  is_valid = (expr.head === :(=) &&
              isa(expr.args[1], SlotNumber) &&
              expr.args[2] === state.temp_loop_var)
  if is_valid
    state.used_loop_var = expr.args[1]
  end

  return is_valid, state
end)

isForLoopUpdateVar = onlyExpr(function(expr::Expr, index::Integer, state::ForLoopData)
  is_valid = (expr.head === :(=) &&
              expr.args[1] === state.loop_var &&
              expr.args[2] === state.inc_loop_var)

  if is_valid
    state.top_index = index
  end

  return is_valid, state
end)

for_loop_changer = Changer(Set(1:0), Dict{Integer, Function}())

function forLoopHeaderValidator(state::ForLoopData, ast::Expr)

  for (i, expr) in enumerate(ast.args)
    # order is important
    if (isForLoopEndGoto(expr, i,  state) &&
        isForLoopEndLabel(ast.args[i + 1], i + 1, state))
      union!(for_loop_changer.ignore,
             Set((state.header_index + 1):state.top_index))
      push!(for_loop_changer.ignore, state.bottom_index + 1)
      for_loop_changer.changers[state.header_index] = function()
        return Expr(:for_loop_start, state.protec_to,
                    state.from, state.to, 1,
                    state.loop_var, state.used_loop_var)
      end
      for_loop_changer.changers[state.bottom_index] = function()
        return Expr(:for_loop_end, state.protec_to)
      end
      break
    end
  end
end

isForLoopEndGoto = onlyGotoNodeB(function(expr::GotoNode, index::Integer, state::ForLoopData)
  is_valid = expr.label == state.begin_label
  if is_valid
    state.bottom_index = index
  end
  return is_valid
end)

isForLoopEndLabel = onlyLabelNodeB(function(expr::LabelNode, index::Integer, state::ForLoopData)
  return state.end_label == expr.label
end)
#### Basic detector

# for_loop_header_pattern = Pattern([onlyExpr(isForLoopHeaderSLE),
#                                    onlyExpr(isForLoopHeaderSubInt),
#                                    onlyExpr(isForLoopHeaderSelect),
#                                    onlyExpr(isForLoopHeaderAddInt)],
#                                   forLoopHeaderValidator)

for_loop_used_pattern = Pattern([isForLoopHeader,
                                   isForLoopVarInit,
                                   isForLoopBeginLabel,
                                   isForLoopTestGoto,
                                   isForLoopVarSave,
                                   isForLoopVarInc,
                                   isForLoopUsedVar,
                                   isForLoopUpdateVar], forLoopHeaderValidator)

for_loop_unused_pattern = Pattern([isForLoopHeader,
                                   isForLoopVarInit,
                                   isForLoopBeginLabel,
                                   isForLoopTestGoto,
                                   isForLoopVarInc,
                                   isForLoopUpdateVar], forLoopHeaderValidator)

for_loop_patterns = [for_loop_used_pattern, for_loop_unused_pattern] #, for_loop_test_pattern]

function detector(ast::Expr)
  remove_unused!(ast)
  detector(ast, for_loop_patterns)
  ast.args = change(ast, [for_loop_changer])
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

function remove_unused!(ast::Expr)
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
