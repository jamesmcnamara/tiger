signature ENV =
sig
    type access
    type ty
    datatype enventry = VarEntry of {access: Translate.access, ty: ty}
                      | FunEntry of {level: Translate.level,
                                     label: Temp.label,
                                     formals: ty list,
                                     result: ty}
    val base_tenv : ty Symbol.table
    val base_venv : enventry Symbol.table
end

structure Env : ENV = struct

type access = unit
type ty = Types.ty

datatype enventry = VarEntry of {access: Translate.access, ty: ty}
                  | FunEntry of {level: Translate.level,
                                 label: Temp.label,
                                 formals: ty list,
                                 result: ty}


fun addtoenv ((symbol, type'), env) =
  Symbol.enter(env, symbol, type')

val base_tenv =
    foldl addtoenv
          Symbol.empty
          [(Symbol.symbol("int"), Types.INT),
           (Symbol.symbol("string"), Types.STRING)]

val base_venv =
    foldl addtoenv Symbol.empty
          [(Symbol.symbol("print"), FunEntry { formals=[Types.STRING],
                                               result= Types.UNIT,
                                               level=Translate.outermost,
                                               label=Temp.newlabel()}),
           (Symbol.symbol("flush"), FunEntry { formals=[],
                                               result= Types.UNIT,
                                               level=Translate.outermost,
                                               label=Temp.newlabel()}),
           (Symbol.symbol("getchar"), FunEntry { formals=[],
                                                 result= Types.STRING,
                                                 level=Translate.outermost,
                                                 label=Temp.newlabel()}),
           (Symbol.symbol("ord"), FunEntry { formals=[Types.STRING],
                                             result= Types.INT,
                                             level=Translate.outermost,
                                             label=Temp.newlabel()}),
           (Symbol.symbol("chr"), FunEntry { formals=[Types.INT],
                                             result= Types.STRING,
                                             level=Translate.outermost,
                                             label=Temp.newlabel()}),
           (Symbol.symbol("size"), FunEntry { formals=[Types.STRING],
                                              result= Types.INT,
                                              level=Translate.outermost,
                                              label=Temp.newlabel()}),
           (Symbol.symbol("substring"), FunEntry { formals=[Types.STRING,
                                                            Types.INT,
                                                            Types.INT],
                                                   result= Types.STRING,
                                                   level=Translate.outermost,
                                                   label=Temp.newlabel()}),
           (Symbol.symbol("concat"), FunEntry { formals=[Types.STRING,
                                                         Types.STRING],
                                                result= Types.STRING,
                                                level=Translate.outermost,
                                                label=Temp.newlabel()}),
           (Symbol.symbol("not"), FunEntry { formals=[Types.INT],
                                             result= Types.INT,
                                             level=Translate.outermost,
                                             label=Temp.newlabel()}),
           (Symbol.symbol("exit"), FunEntry { formals=[Types.INT],
                                              result= Types.UNIT,
                                              level=Translate.outermost,
                                              label=Temp.newlabel()})]

end
