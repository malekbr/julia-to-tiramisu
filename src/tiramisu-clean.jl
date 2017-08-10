module TiramisuClean

using ..ParallelAccelerator

export remove_unused!

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
