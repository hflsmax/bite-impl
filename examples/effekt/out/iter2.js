const $effekt = {  };

const $getOp = "get_882";

const $putOp = "put_883";

function Tuple2_198(first_201, second_202) {
    return {
        __tag: 0,
        __name: "Tuple2",
        __data: [first_201, second_202],
        first_201: first_201,
        second_202: second_202
    };
}

function Tuple3_203(first$1_207, second$1_208, third_209) {
    return {
        __tag: 0,
        __name: "Tuple3",
        __data: [first$1_207, second$1_208, third_209],
        first$1_207: first$1_207,
        second$1_208: second$1_208,
        third_209: third_209
    };
}

function Tuple4_210(first$2_215, second$2_216, third$1_217, fourth_218) {
    return {
        __tag: 0,
        __name: "Tuple4",
        __data: [first$2_215, second$2_216, third$1_217, fourth_218],
        first$2_215: first$2_215,
        second$2_216: second$2_216,
        third$1_217: third$1_217,
        fourth_218: fourth_218
    };
}

function Tuple5_219(first$3_225, second$3_226, third$2_227, fourth$1_228, fifth_229) {
    return {
        __tag: 0,
        __name: "Tuple5",
        __data: [first$3_225, second$3_226, third$2_227, fourth$1_228, fifth_229],
        first$3_225: first$3_225,
        second$3_226: second$3_226,
        third$2_227: third$2_227,
        fourth$1_228: fourth$1_228,
        fifth_229: fifth_229
    };
}

function Tuple6_230(first$4_237, second$4_238, third$3_239, fourth$2_240, fifth$1_241, sixth_242) {
    return {
        __tag: 0,
        __name: "Tuple6",
        __data: [first$4_237, second$4_238, third$3_239, fourth$2_240, fifth$1_241, sixth_242],
        first$4_237: first$4_237,
        second$4_238: second$4_238,
        third$3_239: third$3_239,
        fourth$2_240: fourth$2_240,
        fifth$1_241: fifth$1_241,
        sixth_242: sixth_242
    };
}

function RuntimeError_247() {
    return { __tag: 0, __name: "RuntimeError", __data: [] };
}

function None_384() {
    return { __tag: 0, __name: "None", __data: [] };
}

function Some_385(value_387) {
    return {
        __tag: 1,
        __name: "Some",
        __data: [value_387],
        value_387: value_387
    };
}

function EmptyList_509() {
    return { __tag: 0, __name: "EmptyList", __data: [] };
}

function Nil_510() {
    return { __tag: 0, __name: "Nil", __data: [] };
}

function Cons_511(head$1_514, tail$1_515) {
    return {
        __tag: 1,
        __name: "Cons",
        __data: [head$1_514, tail$1_515],
        head$1_514: head$1_514,
        tail$1_515: tail$1_515
    };
}

const $runtime = (function() {

  // Regions
  function Cell(init) {
    var _value = init;
    const cell = ({
      backup: function() {
        var _backup = _value
        var cell = this;
        return () => { _value = _backup; return cell }
      }
    });
    // $getOp and $putOp are auto generated from the compiler
    cell[$getOp] = function() {
      return _value
    };
    cell[$putOp] = function(v) {
      _value = v;
      return $effekt.unit;
    };
    return cell
  }

  function Arena() {
    return {
      fields: [], // Array[Cell],
      fresh: function(init) {
        const cell = Cell(init)
        this.fields.push(cell)
        return cell;
      },
      backup: function() {
        return this.fields.map(c => c.backup())
      },
      restore: function(backup) {
        this.fields = backup.map(c => c());
        return this
      }
    }
  }

  const global = {
    fresh: function(init) { return Cell(init) }
  }

  // Result -- Trampoline
  function Step(c, k) {
    return { isStep: true, c: c, k: k }
  }
  function trampoline(r) {
    var res = r
    while (res !== null && res !== undefined && res.isStep) {
      res = res.c.apply(res.k)
    }
    return res
  }

  // Lists / Pairs
  function Cons(head, tail) {
    return { head: head, tail: tail }
  }
  const Nil = null

  // Frame = A => Control[B]

  // Metacontinuations / Stacks
  // (frames: List<Frame>, fields: [Cell], prompt: Int, tail: Stack) -> Stack
  function Stack(frames, arena, prompt, tail) {
    return { frames: frames, arena: arena, prompt: prompt, tail: tail }
  }
  function SubStack(frames, arena, backup, prompt, tail) {
    return { frames: frames, arena: arena, backup: backup, prompt: prompt, tail: tail }
  }
  const EmptyStack = null;

  // (stack: Stack<A, B>, a: A) -> Step<B>
  function apply(stack, a) {
    var s = stack;
    while (true) {
      if (s === EmptyStack) return a;
      const fs = s.frames;
      if (fs === Nil) { s = s.tail; continue }
      const result = fs.head(a);
      s.frames = fs.tail;
      return Step(result, s)
    }
  }

  // (subcont: Stack, stack: Stack) -> Stack
  function pushSubcont(subcont, stack) {
    var sub = subcont;
    var s = stack;

    while (sub !== EmptyStack) {
      s = Stack(sub.frames, sub.arena.restore(sub.backup), sub.prompt, s)
      sub = sub.tail
    }
    return s;
  }

  function flatMap(stack, f) {
    if (stack === EmptyStack) { return Stack(Cons(f, Nil), Arena(), null, stack) }
    var fs = stack.frames
    // it should be safe to mutate the frames field, since they are copied in the subcont
    stack.frames = Cons(f, fs)
    return stack
  }

  function splitAt(stack, p) {
    var sub = EmptyStack;
    var s = stack;

    while (s !== EmptyStack) {
      const currentPrompt = s.prompt;
      sub = SubStack(s.frames, s.arena, s.arena.backup(), currentPrompt, sub);
      s = s.tail;
      if (currentPrompt === p) { return Cons(sub, s) }
    }
    throw ("Prompt " + p + " not found")
  }

  function allocateInto(stack, p, cell) {
    var s = stack;

    while (s !== EmptyStack) {
      const currentPrompt = s.prompt
      if (currentPrompt === p) {
        return s.fields.push(cell);
      } else {
        s = s.tail
      }
    }
    throw ("Prompt " + p + " not found")
  }

  function withState(init, f) {
    const cell = Cell(init)
    return Control(k => {
      k.fields.push(cell);
      return Step(f(cell), k)
    })
  }

  function withRegion(prog) {
    return Control(k => {
      return Step(prog(k.arena), k)
    })
  }

  function withStateIn(prompt, init, f) {
    const cell = Cell(init)

    if (prompt === toplevel) {
      return f(cell)
    } else {
      return Control(k => {
        allocateInto(k, prompt, cell);
        return Step(f(cell), k)
      })
    }
  }

  // Delimited Control
  function Control(apply) {
    const self = {
      apply: apply,
      run: () => trampoline(Step(self, Stack(Nil, global, toplevel, EmptyStack))),
      then: f => Control(k => Step(self, flatMap(k, f))),
      state: f => self.then(init => withState(init, f))
    }
    return self
  }

  const pure = a => Control(k => apply(k, a))

  const delayed = a => Control(k => apply(k, a()))

  const shift = p => f => Control(k => {
    const split = splitAt(k, p)
    const localCont = a => Control(k =>
      Step(pure(a), pushSubcont(split.head, k)))
    return Step(f(localCont), split.tail)
  })

  const callcc = f => Control(k => {
    return f(a => trampoline(apply(k, a)))
  })

  const abort = Control(k => $effekt.unit)


  const capture = f => {
    // [abort; f
    const action = () => f($effekt.unit).then(() => abort)
    return shift(toplevel)(k =>
      k({
        shouldRun: false,
        cont : () => k({ shouldRun: true, cont: action })
      })).then(a => a.shouldRun ? a.cont() : $effekt.pure(a.cont))
  }

  const reset = p => c => Control(k => Step(c, Stack(Nil, Arena(), p, k)))

  const toplevel = 1;
  var _prompt = 2;

  function handle(handlers) {
    const p = _prompt++;

    // modify all implementations in the handlers to capture the continuation at prompt p
    const caps = handlers.map(h => {
      var cap = Object.create({})
      for (var op in h) {
        const impl = h[op];
        cap[op] = function() {
          // split two kinds of arguments, parameters of the operation and capabilities
          const args = Array.from(arguments);
          const arity = impl.length - 1
          const oargs = args.slice(0, arity)
          const caps = args.slice(arity)
          var r = shift(p)(k => impl.apply(null, oargs.concat([k])))
          // resume { caps => e}
          if (caps.length > 0) {
            return r.then(f => f.apply(null, caps))
          }
          // resume(v)
          else {
            return r
          }
        }
      }
      return cap;
    });
    return body => reset(p)(body.apply(null, caps))
  }

  return {
    pure: pure,
    callcc: callcc,
    capture: capture,
    delayed: delayed,
    // no lifting for prompt based implementation
    lift: f => f,
    handle: handle,
    fresh: Cell,

    _if: (c, thn, els) => c ? thn() : els(),
    withRegion: withRegion,
    constructor: (_, tag) => function() {
      return { __tag: tag, __data: Array.from(arguments) }
    },

    hole: function() { throw "Implementation missing" }
  }
})()

Object.assign($effekt, $runtime);


function show$impl(obj) {
  if (!!obj && !!obj.__name) {
    return obj.__name + "(" + obj.__data.map(show$impl).join(", ") + ")"
  } else if (!!obj && obj.__unit) {
    return "()";
  } else {
    return "" + obj;
  }
}

function equals$impl(obj1, obj2) {
  if (!!obj1 && !!obj2 && obj1.__data && !!obj2.__data) {
    if (obj1.__tag != obj2.__tag) return false;

    for (var i = 0; i < obj1.__data.length; i++) {
      if (!equals$impl(obj1.__data[i], obj2.__data[i])) return false;
    }
    return true;
  } else {
    return (obj1.__unit && obj2.__unit) || (obj1 === obj2);
  }
}

function println$impl(obj) {
  //return $effekt.delayed(() => { console.log(show(obj)); return $effekt.unit; });
  console.log(show$impl(obj)); return $effekt.unit;
}

$effekt.unit = { __unit: true }

// matchers: Any -> List[Any] | null
const $matching = (function() {

    const any = x => [x]

    const ignore = x => []

    const bind = matcher => x => {
        const matched = matcher(x)
        if (matched == null) return null;
        return [x].concat(matched)
    }

    const literal = c => x => {
        if (equals$impl(c, x))
            return []
        else
            return null
    }

    function tagged(tag) {
        const matchers = arguments
        return x => {
            if (!x || !x.__tag || x.__tag !== tag) return null;
            var extracted = [];
            // we start at 1 since matchers are shifted by 1
            for (var i = 1; i < matchers.length; i++) {
                const matched = matchers[i](x.__data[i - 1]);
                if (matched === null) return null;
                Array.prototype.push.apply(extracted, matched)
            }
            return extracted;
        }
    }

    function match(x, alternatives) {
        for (var i in alternatives) {
            const alt = alternatives[i]
            const matched = alt.pattern(x)
            if (matched !== null) {
                return alt.exec.apply(null, matched)
            }
        }
    }

    return {
        any: any,
        ignore: ignore,
        tagged: tagged,
        bind: bind,
        literal: literal,
        match: match
    }
})();

Object.assign($effekt, $matching);


// p0 = bind(tagged("Nil"))
// p1 = bind(tagged("Cons", any, any))
// p2 = tagged("Cons", any, bind(tagged("Cons", any, ignore)))

// l0 = { tag: "Nil", data: [] }
// l1 = { tag: "Cons", data: [1, l0] }
// l2 = { tag: "Cons", data: [1, { tag: "Cons", data: [2, { tag: "Nil", data: [] }] }] }

// console.log(p1(l0))
// console.log(p1(l1))
// console.log(p1(l2))

// console.log(p2(l0))
// console.log(p2(l1))
// console.log(p2(l2))

// match(l2, [
//     { pattern: p0, exec: () => console.log("It is Nil!") },
//     { pattern: p2, exec: (x, y) => console.log("It has at least two elements", x, y) },
//     { pattern: p1, exec: (x, rest) => console.log("It only has one element", x) }
// ])


function println_6(value) {
    return println$impl(value);
}

function inspect_9(value) {
    return console.log(value);
}

function random_10() {
    return Math.random();
}

function timestamp_11() {
    return Date.now();
}

function freshIn_15(t, r) {
    return r.fresh(t);
}

function freshGlobal_18(t) {
    return $effekt.fresh(t);
}

function show_21(value) {
    return show$impl(value);
}

function infixConcat_24(s1, s2) {
    return s1 + s2;
}

function infixAdd_27(x, y) {
    return (x + y);
}

function infixMul_30(x, y) {
    return (x * y);
}

function infixDiv_33(x, y) {
    return Math.floor(x / y);
}

function infixSub_36(x, y) {
    return (x - y);
}

function mod_39(x, y) {
    return (x % y);
}

function infixAdd$1_42(x, y) {
    return (x + y);
}

function infixMul$1_45(x, y) {
    return (x * y);
}

function infixSub$1_48(x, y) {
    return (x - y);
}

function infixDiv$1_51(x, y) {
    return (x / y);
}

function cos_53(x) {
    return Math.cos(x);
}

function sin_55(x) {
    return Math.sin(x);
}

function atan_57(x) {
    return Math.atan(x);
}

function tan_59(x) {
    return Math.tan(x);
}

function sqrt_61(x) {
    return Math.sqrt(x);
}

function square_63(x) {
    return (x * x);
}

function log_65(x) {
    return Math.log(x);
}

function log1p_67(x) {
    return Math.log1p(x);
}

function exp_69(x) {
    return Math.exp(x);
}

function _pi_70() {
    return Math.PI;
}

function toInt_72(d) {
    return Math.round(d);
}

function toDouble_74(d) {
    return d;
}

function floor_76(d) {
    return Math.floor(d);
}

function ceil_78(d) {
    return Math.ceil(d);
}

function infixEq_82(x, y) {
    return equals$impl(x, y);
}

function infixNeq_86(x, y) {
    return !equals$impl(x, y);
}

function infixLt_89(x, y) {
    return x < y;
}

function infixLte_92(x, y) {
    return x <= y;
}

function infixGt_95(x, y) {
    return x > y;
}

function infixGte_98(x, y) {
    return x >= y;
}

function infixLt$1_101(x, y) {
    return x < y;
}

function infixLte$1_104(x, y) {
    return x <= y;
}

function infixGt$1_107(x, y) {
    return x > y;
}

function infixGte$1_110(x, y) {
    return x >= y;
}

function infixLt$2_113(x, y) {
    return x < y;
}

function infixLte$2_116(x, y) {
    return x <= y;
}

function infixGt$2_119(x, y) {
    return x > y;
}

function infixGte$2_122(x, y) {
    return x >= y;
}

function not_124(b) {
    return !b;
}

function infixOr_127(x, y) {
    return x || y;
}

function infixAnd_130(x, y) {
    return x && y;
}

function isUndefined_133(value) {
    return value === undefined;
}

function isNull_136(value) {
    return value === null;
}

function panic_164(msg) {
    return (function() { throw msg })();
}

function currentTimeNanos_187() {
    return $effekt.delayed(() => new Date().getTime() * 1000);
}

function setTimeout_190(callback, timeout) {
    return (function() { window.setTimeout(() => callback().run(), timeout); return $effekt.unit; })();
}

function locally_3(f_1) {
    return f_1();
}

const PI_197 = _pi_70();

function raise_170(msg_169, Exception$capability_1182) {
    return Exception$capability_1182.raise_246(RuntimeError_247(), msg_169);
}

function panicOn_173(prog_172) {
    return $effekt.handle([{
        raise_246: ((exception_249, msg_250, resume_251) =>
            {
                const tmp1131_1183 = panic_164(msg_250);
                
                return $effekt.pure(tmp1131_1183);
            })
    }])(((Exception$capability_1184) =>
        {
            return prog_172(Exception$capability_1184);
        }));
}

function report_176(prog_175) {
    return $effekt.handle([{
        raise_246: ((exception_253, msg_254, resume_255) =>
            {
                const tmp1136_1185 = println_6(msg_254);
                
                return $effekt.pure(tmp1136_1185);
            })
    }])(((Exception$capability_1186) =>
        {
            return prog_175(Exception$capability_1186);
        }));
}

function ignoring_179(prog_178) {
    return $effekt.handle([{
        raise_246: ((exception_257, msg_258, resume_259) =>
            {
                return $effekt.pure($effekt.unit);
            })
    }])(((Exception$capability_1187) =>
        {
            return prog_178(Exception$capability_1187);
        }));
}

function each_183(start_180, end_181, action_182) {
    return $effekt.withRegion(((this_1155) =>
        {
            const tmp1143_1195 = start_180;
            
            const i_260 = this_1155.fresh(tmp1143_1195);
            
            function tmp1144_1189() {
                const tmp1145_1190 = i_260.get_882();
                
                return ((infixLt_89(tmp1145_1190, end_181)) ? (() =>
                    {
                        const tmp1147_1191 = i_260.get_882();
                        
                        return action_182(tmp1147_1191);
                    })().then((() =>
                    (() =>
                        {
                            const tmp1150_1193 = i_260.get_882();
                            
                            const tmp_1192 = i_260.put_883(infixAdd_27(tmp1150_1193, 1));
                            
                            return $effekt.pure(tmp_1192);
                        })())).then(((tmp1146_1194) => tmp1144_1189())) : $effekt.pure($effekt.unit));
            }
            
            return tmp1144_1189();
        }));
}

function repeat_186(n_184, action_185) {
    return each_183(0, n_184, ((n_261) => { return action_185(); }));
}

function timed_192(block_191) {
    return currentTimeNanos_187().then(((before_262) =>
        block_191().then((() =>
            currentTimeNanos_187().then(((after_263) =>
                $effekt.pure(infixSub_36(after_263, before_262))))))));
}

function measure_196(warmup_193, iterations_194, block_195) {
    function run_266(n_264, report_265) {
        return ((infixLte_92(n_264, 0)) ? $effekt.pure($effekt.unit) : timed_192((() =>
            {
                return block_195();
            })).then(((time_267) =>
            ((report_265) ? (() =>
                {
                    const tmp1172_1196 = println_6(time_267);
                    
                    return $effekt.pure(tmp1172_1196);
                })() : $effekt.pure($effekt.unit)).then(((tmp1173_1197) =>
                (() =>
                    {
                        tmp1173_1197;
                        
                        return run_266(infixSub_36(n_264, 1), report_265);
                    })())))));
    }
    
    return run_266(warmup_193, false).then((() =>
        run_266(iterations_194, true)));
}

function isDefined_354(self_353) {
    const tmp1081_1198 = self_353;
    
    function tmp1082_1199() {
        return $effekt.pure(false);
    }
    
    function tmp1083_1200(v_388) {
        return $effekt.pure(true);
    }
    
    switch (tmp1081_1198.__tag) {
        case 0: return (() => { return tmp1082_1199(); }).apply(null, tmp1081_1198.__data);
        
        case 1: return ((tmp1084_1201) =>
            {
                return tmp1083_1200(tmp1084_1201);
            }).apply(null, tmp1081_1198.__data);
    }
    
    return $effekt.pure(null);
}

function isEmpty_357(self_356) {
    const tmp1087_1202 = isDefined_354(self_356).run();
    
    return $effekt.pure(not_124(tmp1087_1202));
}

function orElse_361(self_359, that_360) {
    const tmp1088_1203 = self_359;
    
    function tmp1091_1204() {
        return that_360();
    }
    
    function tmp1092_1205(v_389) {
        return $effekt.pure(Some_385(v_389));
    }
    
    switch (tmp1088_1203.__tag) {
        case 0: return (() => { return tmp1091_1204(); }).apply(null, tmp1088_1203.__data);
        
        case 1: return ((tmp1093_1206) =>
            {
                return tmp1092_1205(tmp1093_1206);
            }).apply(null, tmp1088_1203.__data);
    }
    
    return $effekt.pure(null);
}

function getOrElse_365(self_363, that_364) {
    const tmp1096_1207 = self_363;
    
    function tmp1099_1208() {
        return that_364();
    }
    
    function tmp1100_1209(v_390) {
        return $effekt.pure(v_390);
    }
    
    switch (tmp1096_1207.__tag) {
        case 0: return (() => { return tmp1099_1208(); }).apply(null, tmp1096_1207.__data);
        
        case 1: return ((tmp1101_1210) =>
            {
                return tmp1100_1209(tmp1101_1210);
            }).apply(null, tmp1096_1207.__data);
    }
    
    return $effekt.pure(null);
}

function map_370(self_368, f_369) {
    const tmp1104_1211 = self_368;
    
    function tmp1105_1212() {
        return $effekt.pure(None_384());
    }
    
    function tmp1107_1213(v_391) {
        return f_369(v_391).then(((tmp1106_1215) =>
            $effekt.pure(Some_385(tmp1106_1215))));
    }
    
    switch (tmp1104_1211.__tag) {
        case 0: return (() => { return tmp1105_1212(); }).apply(null, tmp1104_1211.__data);
        
        case 1: return ((tmp1108_1214) =>
            {
                return tmp1107_1213(tmp1108_1214);
            }).apply(null, tmp1104_1211.__data);
    }
    
    return $effekt.pure(null);
}

function foreach_374(self_372, f_373) {
    const tmp1111_1216 = self_372;
    
    function tmp1112_1217() {
        return $effekt.pure($effekt.unit);
    }
    
    function tmp1115_1218(v_392) {
        return f_373(v_392);
    }
    
    switch (tmp1111_1216.__tag) {
        case 0: return (() => { return tmp1112_1217(); }).apply(null, tmp1111_1216.__data);
        
        case 1: return ((tmp1116_1219) =>
            {
                return tmp1115_1218(tmp1116_1219);
            }).apply(null, tmp1111_1216.__data);
    }
    
    return $effekt.pure(null);
}

function undefinedToOption_377(value_376) {
    return ((isUndefined_133(value_376)) ? $effekt.pure(None_384()) : $effekt.pure(Some_385(value_376)));
}

function nullToOption_380(value_379) {
    return ((isNull_136(value_379)) ? $effekt.pure(None_384()) : $effekt.pure(Some_385(value_379)));
}

function nullishToOption_383(value_382) {
    return ((infixOr_127(isUndefined_133(value_382), isNull_136(value_382))) ? $effekt.pure(None_384()) : $effekt.pure(Some_385(value_382)));
}

function foreach_454(l_452, f_453) {
    return $effekt.withRegion(((this_905) =>
        {
            const tmp884_1229 = l_452;
            
            const remainder_516 = this_905.fresh(tmp884_1229);
            
            function tmp885_1220() {
                const tmp886_1228 = remainder_516.get_882();
                
                const tmp887_1221 = isEmpty_484(tmp886_1228).run();
                
                return ((not_124(tmp887_1221)) ? (() =>
                    {
                        const tmp889_1225 = remainder_516.get_882();
                        
                        const tmp890_1222 = tmp889_1225;
                        
                        function tmp891_1223() {
                            return $effekt.pure($effekt.unit);
                        }
                        
                        function tmp896_1224(a_517, as_518) {
                            return f_453(a_517).then((() =>
                                (() =>
                                    {
                                        const tmp_1226 = remainder_516.put_883(as_518);
                                        
                                        return $effekt.pure(tmp_1226);
                                    })()));
                        }
                        
                        switch (tmp890_1222.__tag) {
                            case 0: return (() => { return tmp891_1223(); }).apply(null, tmp890_1222.__data);
                            
                            case 1: return ((tmp897_900, tmp898_899) =>
                                {
                                    return tmp896_1224(tmp897_900, tmp898_899);
                                }).apply(null, tmp890_1222.__data);
                        }
                        
                        return $effekt.pure(null);
                    })().then(((tmp888_1227) => tmp885_1220())) : $effekt.pure($effekt.unit));
            }
            
            return tmp885_1220();
        }));
}

function map_459(l_457, f_458) {
    return $effekt.withRegion(((this_918) =>
        {
            const tmp908_1234 = Nil_510();
            
            const acc_519 = this_918.fresh(tmp908_1234);
            
            return foreach_454(l_457, ((el_520) =>
                {
                    return f_458(el_520).then(((tmp909_1230) =>
                        (() =>
                            {
                                const tmp910_1232 = acc_519.get_882();
                                
                                const tmp_1231 = acc_519.put_883(Cons_511(tmp909_1230, tmp910_1232));
                                
                                return $effekt.pure(tmp_1231);
                            })()));
                })).then((() =>
                (() =>
                    {
                        const tmp915_1233 = acc_519.get_882();
                        
                        return reverse_465(tmp915_1233);
                    })()));
        }));
}

function size_462(l_461) {
    return $effekt.withRegion(((this_927) =>
        {
            const tmp921_1238 = 0;
            
            const n_521 = this_927.fresh(tmp921_1238);
            
            foreach_454(l_461, ((__522) =>
                {
                    const tmp922_1237 = n_521.get_882();
                    
                    const tmp_1236 = n_521.put_883(infixAdd_27(tmp922_1237, 1));
                    
                    return $effekt.pure(tmp_1236);
                })).run();
            
            const tmp_1235 = n_521.get_882();
            
            return $effekt.pure(tmp_1235);
        }));
}

function reverse_465(l_464) {
    return $effekt.withRegion(((this_936) =>
        {
            const tmp930_1242 = Nil_510();
            
            const res_523 = this_936.fresh(tmp930_1242);
            
            foreach_454(l_464, ((el_524) =>
                {
                    const tmp931_1241 = res_523.get_882();
                    
                    const tmp_1240 = res_523.put_883(Cons_511(el_524, tmp931_1241));
                    
                    return $effekt.pure(tmp_1240);
                })).run();
            
            const tmp_1239 = res_523.get_882();
            
            return $effekt.pure(tmp_1239);
        }));
}

function reverseOnto_469(l_467, other_468) {
    const tmp939_1243 = l_467;
    
    function tmp940_1244() {
        return $effekt.pure(other_468);
    }
    
    function tmp943_1245(a_525, rest_526) {
        return reverseOnto_469(rest_526, Cons_511(a_525, other_468));
    }
    
    switch (tmp939_1243.__tag) {
        case 0: return (() => { return tmp940_1244(); }).apply(null, tmp939_1243.__data);
        
        case 1: return ((tmp944_947, tmp945_946) =>
            {
                return tmp943_1245(tmp944_947, tmp945_946);
            }).apply(null, tmp939_1243.__data);
    }
    
    return $effekt.pure(null);
}

function append_473(l_471, other_472) {
    const tmp950_1246 = reverse_465(l_471).run();
    
    return reverseOnto_469(tmp950_1246, other_472);
}

function take_477(l_475, n_476) {
    return ((infixEq_82(n_476, 0)) ? $effekt.pure(Nil_510()) : (() =>
        {
            const tmp953_1247 = l_475;
            
            function tmp954_1248() {
                return $effekt.pure(Nil_510());
            }
            
            function tmp956_1249(a_527, rest_528) {
                const tmp955_1250 = take_477(rest_528, infixSub_36(n_476, 1)).run();
                
                return $effekt.pure(Cons_511(a_527, tmp955_1250));
            }
            
            switch (tmp953_1247.__tag) {
                case 0: return (() => { return tmp954_1248(); }).apply(null, tmp953_1247.__data);
                
                case 1: return ((tmp957_960, tmp958_959) =>
                    {
                        return tmp956_1249(tmp957_960, tmp958_959);
                    }).apply(null, tmp953_1247.__data);
            }
            
            return $effekt.pure(null);
        })());
}

function drop_481(l_479, n_480) {
    return ((infixEq_82(n_480, 0)) ? $effekt.pure(l_479) : (() =>
        {
            const tmp965_1251 = l_479;
            
            function tmp966_1252() {
                return $effekt.pure(Nil_510());
            }
            
            function tmp969_1253(a_529, rest_530) {
                return drop_481(rest_530, infixSub_36(n_480, 1));
            }
            
            switch (tmp965_1251.__tag) {
                case 0: return (() => { return tmp966_1252(); }).apply(null, tmp965_1251.__data);
                
                case 1: return ((tmp970_973, tmp971_972) =>
                    {
                        return tmp969_1253(tmp970_973, tmp971_972);
                    }).apply(null, tmp965_1251.__data);
            }
            
            return $effekt.pure(null);
        })());
}

function isEmpty_484(l_483) {
    const tmp978_1254 = l_483;
    
    function tmp979_1255() {
        return $effekt.pure(true);
    }
    
    function tmp980_1256(a_531, rest_532) {
        return $effekt.pure(false);
    }
    
    switch (tmp978_1254.__tag) {
        case 0: return (() => { return tmp979_1255(); }).apply(null, tmp978_1254.__data);
        
        case 1: return ((tmp981_984, tmp982_983) =>
            {
                return tmp980_1256(tmp981_984, tmp982_983);
            }).apply(null, tmp978_1254.__data);
    }
    
    return $effekt.pure(null);
}

function nonEmpty_487(l_486) {
    const tmp987_1257 = l_486;
    
    function tmp988_1258() {
        return $effekt.pure(false);
    }
    
    function tmp989_1259(a_533, rest_534) {
        return $effekt.pure(true);
    }
    
    switch (tmp987_1257.__tag) {
        case 0: return (() => { return tmp988_1258(); }).apply(null, tmp987_1257.__data);
        
        case 1: return ((tmp990_993, tmp991_992) =>
            {
                return tmp989_1259(tmp990_993, tmp991_992);
            }).apply(null, tmp987_1257.__data);
    }
    
    return $effekt.pure(null);
}

function head_490(l_489, Exception$capability_1263) {
    const tmp996_1260 = l_489;
    
    function tmp999_1261() {
        return Exception$capability_1263.raise_246(EmptyList_509(), "Trying to get the head of an empty list");
    }
    
    function tmp1000_1262(a_535, rest_536) {
        return $effekt.pure(a_535);
    }
    
    switch (tmp996_1260.__tag) {
        case 0: return (() => { return tmp999_1261(); }).apply(null, tmp996_1260.__data);
        
        case 1: return ((tmp1001_1004, tmp1002_1003) =>
            {
                return tmp1000_1262(tmp1001_1004, tmp1002_1003);
            }).apply(null, tmp996_1260.__data);
    }
    
    return $effekt.pure(null);
}

function tail_493(l_492, Exception$capability_1267) {
    const tmp1007_1264 = l_492;
    
    function tmp1010_1265() {
        return Exception$capability_1267.raise_246(EmptyList_509(), "Trying to get the head of an empty list");
    }
    
    function tmp1011_1266(a_537, rest_538) {
        return $effekt.pure(rest_538);
    }
    
    switch (tmp1007_1264.__tag) {
        case 0: return (() => { return tmp1010_1265(); }).apply(null, tmp1007_1264.__data);
        
        case 1: return ((tmp1012_1015, tmp1013_1014) =>
            {
                return tmp1011_1266(tmp1012_1015, tmp1013_1014);
            }).apply(null, tmp1007_1264.__data);
    }
    
    return $effekt.pure(null);
}

function headOption_496(l_495) {
    const tmp1018_1268 = l_495;
    
    function tmp1019_1269() {
        return $effekt.pure(None_384());
    }
    
    function tmp1020_1270(a_539, rest_540) {
        return $effekt.pure(Some_385(a_539));
    }
    
    switch (tmp1018_1268.__tag) {
        case 0: return (() => { return tmp1019_1269(); }).apply(null, tmp1018_1268.__data);
        
        case 1: return ((tmp1021_1024, tmp1022_1023) =>
            {
                return tmp1020_1270(tmp1021_1024, tmp1022_1023);
            }).apply(null, tmp1018_1268.__data);
    }
    
    return $effekt.pure(null);
}

function partition_500(l_498, pred_499) {
    return $effekt.withRegion(((this_1044) =>
        {
            const tmp1027_1281 = Nil_510();
            
            const lefts_541 = this_1044.fresh(tmp1027_1281);
            
            const tmp1028_1280 = Nil_510();
            
            const rights_542 = this_1044.fresh(tmp1028_1280);
            
            return foreach_454(l_498, ((el_543) =>
                {
                    return pred_499(el_543).then(((tmp1029_1271) =>
                        ((tmp1029_1271) ? (() =>
                            {
                                const tmp1030_1273 = lefts_541.get_882();
                                
                                const tmp_1272 = lefts_541.put_883(Cons_511(el_543, tmp1030_1273));
                                
                                return $effekt.pure(tmp_1272);
                            })() : (() =>
                            {
                                const tmp1033_1275 = rights_542.get_882();
                                
                                const tmp_1274 = rights_542.put_883(Cons_511(el_543, tmp1033_1275));
                                
                                return $effekt.pure(tmp_1274);
                            })())));
                })).then((() =>
                (() =>
                    {
                        const tmp1040_1279 = lefts_541.get_882();
                        
                        const tmp1041_1276 = reverse_465(tmp1040_1279).run();
                        
                        const tmp1042_1278 = rights_542.get_882();
                        
                        const tmp1043_1277 = reverse_465(tmp1042_1278).run();
                        
                        return $effekt.pure(Tuple2_198(tmp1041_1276, tmp1043_1277));
                    })()));
        }));
}

function sortBy_504(l_502, compare_503) {
    const tmp1047_1282 = l_502;
    
    function tmp1048_1283() {
        return $effekt.pure(Nil_510());
    }
    
    function tmp1070_1284(pivot_544, rest_545) {
        return partition_500(rest_545, ((el_546) =>
            {
                return compare_503(el_546, pivot_544);
            })).then(((tmp1051_1285) =>
            (() =>
                {
                    const tmp1052_1286 = tmp1051_1285;
                    
                    function tmp1063_1287(lt_547, gt_548) {
                        return sortBy_504(lt_547, ((a_549, b_550) =>
                            {
                                return compare_503(a_549, b_550);
                            })).then(((leftSorted_551) =>
                            sortBy_504(gt_548, ((a_552, b_553) =>
                                {
                                    return compare_503(a_552, b_553);
                                })).then(((rightSorted_554) =>
                                append_473(leftSorted_551, Cons_511(pivot_544, rightSorted_554))))));
                    }
                    
                    switch (tmp1052_1286.__tag) {
                        case 0: return ((tmp1064_1067, tmp1065_1066) =>
                            {
                                return tmp1063_1287(tmp1064_1067, tmp1065_1066);
                            }).apply(null, tmp1052_1286.__data);
                    }
                    
                    return $effekt.pure(null);
                })()));
    }
    
    switch (tmp1047_1282.__tag) {
        case 0: return (() => { return tmp1048_1283(); }).apply(null, tmp1047_1282.__data);
        
        case 1: return ((tmp1071_1074, tmp1072_1073) =>
            {
                return tmp1070_1284(tmp1071_1074, tmp1072_1073);
            }).apply(null, tmp1047_1282.__data);
    }
    
    return $effekt.pure(null);
}

function sort_506(l_505) {
    return sortBy_504(l_505, ((a_555, b_556) =>
        {
            return $effekt.pure(infixLt_89(a_555, b_556));
        }));
}

function sort$1_508(l_507) {
    return sortBy_504(l_507, ((a_557, b_558) =>
        {
            return $effekt.pure(infixLt$1_101(a_557, b_558));
        }));
}

function iter_732(l_731, Yield$capability_1296, Behead$capability_1306) {
    return $effekt.withRegion(((this_853) =>
        {
            const tmp815_1305 = l_731;
            
            const in_739 = this_853.fresh(tmp815_1305);
            
            const tmp816_1304 = Nil_510();
            
            const out_740 = this_853.fresh(tmp816_1304);
            
            return (() =>
                {
                    function tmp817_1288() {
                        const tmp818_1302 = in_739.get_882();
                        
                        const tmp819_1289 = isEmpty_484(tmp818_1302).run();
                        
                        return ((not_124(tmp819_1289)) ? ignoring_179(((Exception$capability_1291) =>
                            {
                                return $effekt.withRegion(((this_843) =>
                                    {
                                        return (() =>
                                            {
                                                const tmp821_1290 = in_739.get_882();
                                                
                                                return head_490(tmp821_1290, Exception$capability_1291);
                                            })().then(((tmp824_1292) =>
                                            (() =>
                                                {
                                                    const hd_741 = this_843.fresh(tmp824_1292);
                                                    
                                                    return (() =>
                                                        {
                                                            const tmp825_1293 = in_739.get_882();
                                                            
                                                            return tail_493(tmp825_1293, Exception$capability_1291);
                                                        })().then(((tl_742) =>
                                                        $effekt.handle([{
                                                            Replace_737: ((n_743, resume_744) =>
                                                                {
                                                                    const tmp835_1294 = out_740.get_882();
                                                                    
                                                                    const tmp836_1295 = out_740.put_883(Cons_511(n_743, tmp835_1294));
                                                                    
                                                                    tmp836_1295;
                                                                    
                                                                    return resume_744($effekt.unit);
                                                                })
                                                        }, {
                                                            Behead_738: ((resume_745) =>
                                                                {
                                                                    return resume_745($effekt.unit);
                                                                })
                                                        }])(((Replace$capability_1298, Behead$capability_1299) =>
                                                            {
                                                                return (() =>
                                                                    {
                                                                        const tmp828_1297 = hd_741.get_882();
                                                                        
                                                                        return Yield$capability_1296.Yield_735(tmp828_1297, Replace$capability_1298, Behead$capability_1299);
                                                                    })().then((() =>
                                                                    (() =>
                                                                        {
                                                                            const tmp_1300 = in_739.put_883(tl_742);
                                                                            
                                                                            return $effekt.pure(tmp_1300);
                                                                        })()));
                                                            }))));
                                                })()));
                                    }));
                            })).then(((tmp820_1301) => tmp817_1288())) : $effekt.pure($effekt.unit));
                    }
                    
                    return tmp817_1288();
                })().then((() =>
                (() =>
                    {
                        const tmp850_1303 = out_740.get_882();
                        
                        return reverse_465(tmp850_1303);
                    })()));
        }));
}

function main_733() {
    return ignoring_179(((Exception$capability_1310) =>
        {
            return $effekt.withRegion(((this_877) =>
                {
                    const tmp856_1317 = Cons_511(0, Cons_511(1, Cons_511(3, Cons_511(infixSub_36(0, 2), Cons_511(infixSub_36(0, 8), Cons_511(9, Nil_510()))))));
                    
                    const lst_746 = this_877.fresh(tmp856_1317);
                    
                    return $effekt.handle([{
                        Yield_735: ((x_747, resume_748) =>
                            {
                                return resume_748(((Replace$capability_1308, Behead$capability_1307) =>
                                    {
                                        return ((infixLt_89(x_747, 0)) ? Behead$capability_1307.Behead_738() : Replace$capability_1308.Replace_737(infixMul_30(x_747, 2)));
                                    }));
                            })
                    }, {
                        Behead_738: ((resume_749) =>
                            {
                                println_6("beheading in client?");
                                
                                return (() =>
                                    {
                                        const tmp868_1309 = lst_746.get_882();
                                        
                                        return tail_493(tmp868_1309, Exception$capability_1310).then(((tmp869_1311) =>
                                            (() =>
                                                {
                                                    const tmp_1312 = lst_746.put_883(tmp869_1311);
                                                    
                                                    return $effekt.pure(tmp_1312);
                                                })()));
                                    })().then((() =>
                                    resume_749($effekt.unit)));
                            })
                    }])(((Yield$capability_1314, Behead$capability_1315) =>
                        {
                            const tmp857_1313 = lst_746.get_882();
                            
                            return iter_732(tmp857_1313, Yield$capability_1314, Behead$capability_1315);
                        })).then(((res_750) =>
                        (() =>
                            {
                                const tmp876_1316 = println_6(res_750);
                                
                                return $effekt.pure(tmp876_1316);
                            })()));
                }));
        }));
}

module.exports = { main: main_733 };