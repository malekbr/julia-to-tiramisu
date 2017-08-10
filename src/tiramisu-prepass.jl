module TiramisuPrepass
export detector

using ..ParallelAccelerator

SupportedNode = Union{LabelNode, GotoNode, Expr, NewvarNode, LineNumberNode,
                      SlotNumber}

mk_expr(sym, varargs...) = Expr(:call, GlobalRef(TiramisuPrepass, sym),
                                varargs...)

type Future
  matcher::Function
end

Matcher = Union{Function, Future}



# A matcher takes three variables:
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

function onlyGotoNode(f::Function)
  return function(node::SupportedNode, index::Integer, state::State)
    if !(typeof(node) <: GotoNode)
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
  id::Union{Void, Integer} # assigned id
  from::Union{Void, Integer} # lower bound of loop
  to::Union{Void, Integer} # upper bound of loop
  protec_to::Union{Void, SSAValue} # Computed upper bound of loop
  loop_var::Union{Void, SlotNumber} # Actual loop variable
  used_loop_var::Union{Void, SlotNumber} # Exposed loop variable (if variable is used)
  begin_label::Union{Void, Int64} # Label for reloop
  cont_label::Union{Void, Int64} # Label for continue
  end_label::Union{Void, Int64} # Label to end loop
  temp_loop_var::Union{Void, SSAValue} # In case of used_loop_var, original value stored here
  inc_loop_var::Union{Void, SSAValue} # In case of used_loop_var, original value stored here
  header_index::Union{Void, Integer} # Index of loop header
  top_index::Union{Void, Integer}
  bottom_index::Union{Void, Integer} # Index of Goto at loop end
end

isForLoopMeta = onlyExpr(function(expr::Expr, index::Integer, state::NoState)
  is_valid = expr.head === :meta && expr.args[1] === :forloop

  if is_valid
    state = ForLoopData(repeated(nothing, length(fieldnames(ForLoopData)))...)
    state.id = expr.args[2]
    state.header_index = index
    return true, state
  end
  return false, NoState()
end)

isForLoopHeader = onlyExpr(function(expr::Expr, index::Integer, state::ForLoopData)
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
    state.from = from
    state.to = to
    state.protec_to = expr.args[1]
  end

  return is_valid, state
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
        isForLoopContLabel(ast.args[i - 1], i - 1,  state) &&
        isForLoopEndLabel(ast.args[i + 1], i + 1, state))
      union!(for_loop_changer.ignore,
             Set((state.header_index + 1):state.top_index))
      push!(for_loop_changer.ignore, state.bottom_index - 1)
      push!(for_loop_changer.ignore, state.bottom_index + 1)
      for_loop_changer.changers[state.header_index] = function()
        return mk_expr(:for_loop_start, state.id,
                       state.from, state.to, 1,
                       state.loop_var, state.used_loop_var)
      end
      for_loop_changer.changers[state.bottom_index] = function()
        return mk_expr(:for_loop_end, state.id)
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

isForLoopContLabel = onlyLabelNodeB(function(expr::LabelNode, index::Integer, state::ForLoopData)
  state.cont_label = expr.label
  return true
end)

isForLoopEndLabel = onlyLabelNodeB(function(expr::LabelNode, index::Integer, state::ForLoopData)
  return state.end_label == expr.label
end)

type IfData <: State
  id::Union{Void, Integer}
  condition::Any
  changer::Changer
end

isIfMeta = onlyExpr(function(expr::Expr, index::Integer, state::NoState)
  is_valid = expr.head === :meta && expr.args[1] === :if
  if is_valid
    state = IfData(expr.args[2], nothing, Changer(Set(), Dict()))
    push!(state.changer.ignore, index)
  end
  return is_valid, state
end)

isIfGotoIfNot = Future(onlyExpr(function(expr::Expr, index::Integer, state::IfData)
  is_valid = expr.head === :gotoifnot
  if is_valid
    state.condition = expr.args[1]
    state.changer.changers[index] = () -> mk_expr(:if, state.id, state.condition)
  end
  return is_valid, state
end))

if_changer = Changer(Set(), Dict())

function ifValidator(state::IfData, ast::Expr)
  union!(if_changer.ignore, state.changer.ignore)
  merge!(if_changer.changers, state.changer.changers)
end

type ContinueData <: State
  id::Union{Void, Integer}
  changer::Changer
end

isContinueMeta = onlyExpr(function(expr::Expr, index::Integer, state::NoState)
  is_valid = expr.head === :meta && expr.args[1] === :continue
  if is_valid
    state = ContinueData(expr.args[2], Changer(Set(), Dict()))
    state.changer.changers[index] = () -> mk_expr(:continue, state.id)
  end
  return is_valid, state
end)

isContinueGoto = onlyGotoNode(function(expr::GotoNode, index::Integer, state::ContinueData)
  push!(state.changer.ignore, index)
  return true, state
end)

continue_changer = Changer(Set(), Dict())

function continueValidator(state::ContinueData, ast::Expr)
  union!(continue_changer.ignore, state.changer.ignore)
  merge!(continue_changer.changers, state.changer.changers)
end

type EndifData <: State
  id::Union{Void, Integer}
  changer::Changer
end

isEndifMeta = onlyExpr(function(expr::Expr, index::Integer, state::NoState)
  is_valid = expr.head === :meta && expr.args[1] === :endif
  if is_valid
    state = EndifData(expr.args[2], Changer(Set(), Dict()))
    state.changer.changers[index] = () -> mk_expr(:endif, state.id)
  end
  return is_valid, state
end)

isEndifLabel = onlyLabelNode(function(expr::LabelNode, index::Integer, state::EndifData)
  push!(state.changer.ignore, index)
  return true, state
end)

endif_changer = Changer(Set(), Dict())

function endifValidator(state::EndifData, ast::Expr)
  union!(endif_changer.ignore, state.changer.ignore)
  merge!(endif_changer.changers, state.changer.changers)
end

type ElseData <: State
  id::Union{Void, Integer}
  changer::Changer
end

isElseGoto = onlyGotoNode(function(expr::GotoNode, index::Integer, state::NoState)
  state = ElseData(nothing, Changer(Set(), Dict()))
  push!(state.changer.ignore, index)
  return true, state
end)

isElseLabel = onlyLabelNode(function(expr::LabelNode, index::Integer, state::ElseData)
  push!(state.changer.ignore, index)
  return true, state
end)

isElseMeta = onlyExpr(function(expr::Expr, index::Integer, state::ElseData)
  is_valid = expr.head === :meta && expr.args[1] === :else
  if is_valid
    state.id = expr.args[2]
    state.changer.changers[index] = () -> mk_expr(:else, state.id)
  end
  return is_valid, state
end)

else_changer = Changer(Set(), Dict())

function elseValidator(state::ElseData, ast::Expr)
  union!(else_changer.ignore, state.changer.ignore)
  merge!(else_changer.changers, state.changer.changers)
end
###########

for_loop_used_pattern = Pattern([isForLoopMeta,
                                 isForLoopHeader,
                                 isForLoopVarInit,
                                 isForLoopBeginLabel,
                                 isForLoopTestGoto,
                                 isForLoopVarSave,
                                 isForLoopVarInc,
                                 isForLoopUsedVar,
                                 isForLoopUpdateVar], forLoopHeaderValidator)

for_loop_unused_pattern = Pattern([isForLoopMeta,
                                   isForLoopHeader,
                                   isForLoopVarInit,
                                   isForLoopBeginLabel,
                                   isForLoopTestGoto,
                                   isForLoopVarInc,
                                   isForLoopUpdateVar], forLoopHeaderValidator)
if_pattern = Pattern([isIfMeta, isIfGotoIfNot], ifValidator)

continue_pattern = Pattern([isContinueMeta, isContinueGoto], continueValidator)

endif_pattern = Pattern([isEndifMeta, isEndifLabel], endifValidator)

else_pattern = Pattern([isElseGoto, isElseLabel, isElseMeta], elseValidator)

patterns = [for_loop_used_pattern, for_loop_unused_pattern, if_pattern,
            continue_pattern, endif_pattern, else_pattern] #, for_loop_test_pattern]

changers = [for_loop_changer, if_changer, continue_changer, endif_changer,
            else_changer]

function detector(ast::Expr)
  detector(ast, patterns)
  change!(ast, changers)
end

function change!(ast::Expr, changers::Vector{Changer})
  new_args = Vector{Any}()
  for (i, e) in enumerate(ast.args)
    ignore = any(i in c.ignore for c in changers)
    new_nodes = [c.changers[i]() for c in changers if haskey(c.changers, i)]
    # TODO store level from pretag, sort changers by level, asc or descending
    new_args = vcat(new_args, new_nodes)

    if !ignore && isempty(new_nodes)
      push!(new_args, e)
    end
  end
  ast.args = new_args
end

function call_matcher(f::Function, params)
  return f(params...)..., false
end

function call_matcher(f::Future, params)
  return f.matcher(params...)..., true
end

function detector(ast::Expr, patterns::Vector{Pattern})
  states = State[NoState() for state in patterns]
  indices = [1 for expr in patterns]
  for (e_i, expr) in enumerate(ast.args)
    if isa(expr, LineNumberNode)
      continue
    end
    for (p_i, pattern) in enumerate(patterns)
      @label pattern_check
      matched, state, future = call_matcher(pattern.matchers[indices[p_i]],
                                            (expr, e_i, states[p_i]))
      if matched
        if indices[p_i] == length(pattern.matchers)
          # pattern matched!
          pattern.validator(state, ast)

          states[p_i] = NoState()
          indices[p_i] = 1
        else
          # pattern not matched yet
          indices[p_i] += 1
          states[p_i] = state
        end
      elseif !future
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

end
