const $effekt = {  };

const $getOp = "get_875";

const $putOp = "put_876";

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

function raise_170(msg_169, Exception$capability_1175) {
    return Exception$capability_1175.raise_246(RuntimeError_247(), msg_169);
}

function panicOn_173(prog_172) {
    return $effekt.handle([{
        raise_246: ((exception_249, msg_250, resume_251) =>
            {
                const tmp1124_1176 = panic_164(msg_250);
                
                return $effekt.pure(tmp1124_1176);
            })
    }])(((Exception$capability_1177) =>
        {
            return prog_172(Exception$capability_1177);
        }));
}

function report_176(prog_175) {
    return $effekt.handle([{
        raise_246: ((exception_253, msg_254, resume_255) =>
            {
                const tmp1129_1178 = println_6(msg_254);
                
                return $effekt.pure(tmp1129_1178);
            })
    }])(((Exception$capability_1179) =>
        {
            return prog_175(Exception$capability_1179);
        }));
}

function ignoring_179(prog_178) {
    return $effekt.handle([{
        raise_246: ((exception_257, msg_258, resume_259) =>
            {
                return $effekt.pure($effekt.unit);
            })
    }])(((Exception$capability_1180) =>
        {
            return prog_178(Exception$capability_1180);
        }));
}

function each_183(start_180, end_181, action_182) {
    return $effekt.withRegion(((this_1148) =>
        {
            const tmp1136_1188 = start_180;
            
            const i_260 = this_1148.fresh(tmp1136_1188);
            
            function tmp1137_1182() {
                const tmp1138_1183 = i_260.get_875();
                
                return ((infixLt_89(tmp1138_1183, end_181)) ? (() =>
                    {
                        const tmp1140_1184 = i_260.get_875();
                        
                        return action_182(tmp1140_1184);
                    })().then((() =>
                    (() =>
                        {
                            const tmp1143_1186 = i_260.get_875();
                            
                            const tmp_1185 = i_260.put_876(infixAdd_27(tmp1143_1186, 1));
                            
                            return $effekt.pure(tmp_1185);
                        })())).then(((tmp1139_1187) => tmp1137_1182())) : $effekt.pure($effekt.unit));
            }
            
            return tmp1137_1182();
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
                    const tmp1165_1189 = println_6(time_267);
                    
                    return $effekt.pure(tmp1165_1189);
                })() : $effekt.pure($effekt.unit)).then(((tmp1166_1190) =>
                (() =>
                    {
                        tmp1166_1190;
                        
                        return run_266(infixSub_36(n_264, 1), report_265);
                    })())))));
    }
    
    return run_266(warmup_193, false).then((() =>
        run_266(iterations_194, true)));
}

function isDefined_354(self_353) {
    const tmp1074_1191 = self_353;
    
    function tmp1075_1192() {
        return $effekt.pure(false);
    }
    
    function tmp1076_1193(v_388) {
        return $effekt.pure(true);
    }
    
    switch (tmp1074_1191.__tag) {
        case 0: return (() => { return tmp1075_1192(); }).apply(null, tmp1074_1191.__data);
        
        case 1: return ((tmp1077_1194) =>
            {
                return tmp1076_1193(tmp1077_1194);
            }).apply(null, tmp1074_1191.__data);
    }
    
    return $effekt.pure(null);
}

function isEmpty_357(self_356) {
    const tmp1080_1195 = isDefined_354(self_356).run();
    
    return $effekt.pure(not_124(tmp1080_1195));
}

function orElse_361(self_359, that_360) {
    const tmp1081_1196 = self_359;
    
    function tmp1084_1197() {
        return that_360();
    }
    
    function tmp1085_1198(v_389) {
        return $effekt.pure(Some_385(v_389));
    }
    
    switch (tmp1081_1196.__tag) {
        case 0: return (() => { return tmp1084_1197(); }).apply(null, tmp1081_1196.__data);
        
        case 1: return ((tmp1086_1199) =>
            {
                return tmp1085_1198(tmp1086_1199);
            }).apply(null, tmp1081_1196.__data);
    }
    
    return $effekt.pure(null);
}

function getOrElse_365(self_363, that_364) {
    const tmp1089_1200 = self_363;
    
    function tmp1092_1201() {
        return that_364();
    }
    
    function tmp1093_1202(v_390) {
        return $effekt.pure(v_390);
    }
    
    switch (tmp1089_1200.__tag) {
        case 0: return (() => { return tmp1092_1201(); }).apply(null, tmp1089_1200.__data);
        
        case 1: return ((tmp1094_1203) =>
            {
                return tmp1093_1202(tmp1094_1203);
            }).apply(null, tmp1089_1200.__data);
    }
    
    return $effekt.pure(null);
}

function map_370(self_368, f_369) {
    const tmp1097_1204 = self_368;
    
    function tmp1098_1205() {
        return $effekt.pure(None_384());
    }
    
    function tmp1100_1206(v_391) {
        return f_369(v_391).then(((tmp1099_1208) =>
            $effekt.pure(Some_385(tmp1099_1208))));
    }
    
    switch (tmp1097_1204.__tag) {
        case 0: return (() => { return tmp1098_1205(); }).apply(null, tmp1097_1204.__data);
        
        case 1: return ((tmp1101_1207) =>
            {
                return tmp1100_1206(tmp1101_1207);
            }).apply(null, tmp1097_1204.__data);
    }
    
    return $effekt.pure(null);
}

function foreach_374(self_372, f_373) {
    const tmp1104_1209 = self_372;
    
    function tmp1105_1210() {
        return $effekt.pure($effekt.unit);
    }
    
    function tmp1108_1211(v_392) {
        return f_373(v_392);
    }
    
    switch (tmp1104_1209.__tag) {
        case 0: return (() => { return tmp1105_1210(); }).apply(null, tmp1104_1209.__data);
        
        case 1: return ((tmp1109_1212) =>
            {
                return tmp1108_1211(tmp1109_1212);
            }).apply(null, tmp1104_1209.__data);
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
    return $effekt.withRegion(((this_898) =>
        {
            const tmp877_1222 = l_452;
            
            const remainder_516 = this_898.fresh(tmp877_1222);
            
            function tmp878_1213() {
                const tmp879_1221 = remainder_516.get_875();
                
                const tmp880_1214 = isEmpty_484(tmp879_1221).run();
                
                return ((not_124(tmp880_1214)) ? (() =>
                    {
                        const tmp882_1218 = remainder_516.get_875();
                        
                        const tmp883_1215 = tmp882_1218;
                        
                        function tmp884_1216() {
                            return $effekt.pure($effekt.unit);
                        }
                        
                        function tmp889_1217(a_517, as_518) {
                            return f_453(a_517).then((() =>
                                (() =>
                                    {
                                        const tmp_1219 = remainder_516.put_876(as_518);
                                        
                                        return $effekt.pure(tmp_1219);
                                    })()));
                        }
                        
                        switch (tmp883_1215.__tag) {
                            case 0: return (() => { return tmp884_1216(); }).apply(null, tmp883_1215.__data);
                            
                            case 1: return ((tmp890_893, tmp891_892) =>
                                {
                                    return tmp889_1217(tmp890_893, tmp891_892);
                                }).apply(null, tmp883_1215.__data);
                        }
                        
                        return $effekt.pure(null);
                    })().then(((tmp881_1220) => tmp878_1213())) : $effekt.pure($effekt.unit));
            }
            
            return tmp878_1213();
        }));
}

function map_459(l_457, f_458) {
    return $effekt.withRegion(((this_911) =>
        {
            const tmp901_1227 = Nil_510();
            
            const acc_519 = this_911.fresh(tmp901_1227);
            
            return foreach_454(l_457, ((el_520) =>
                {
                    return f_458(el_520).then(((tmp902_1223) =>
                        (() =>
                            {
                                const tmp903_1225 = acc_519.get_875();
                                
                                const tmp_1224 = acc_519.put_876(Cons_511(tmp902_1223, tmp903_1225));
                                
                                return $effekt.pure(tmp_1224);
                            })()));
                })).then((() =>
                (() =>
                    {
                        const tmp908_1226 = acc_519.get_875();
                        
                        return reverse_465(tmp908_1226);
                    })()));
        }));
}

function size_462(l_461) {
    return $effekt.withRegion(((this_920) =>
        {
            const tmp914_1231 = 0;
            
            const n_521 = this_920.fresh(tmp914_1231);
            
            foreach_454(l_461, ((__522) =>
                {
                    const tmp915_1230 = n_521.get_875();
                    
                    const tmp_1229 = n_521.put_876(infixAdd_27(tmp915_1230, 1));
                    
                    return $effekt.pure(tmp_1229);
                })).run();
            
            const tmp_1228 = n_521.get_875();
            
            return $effekt.pure(tmp_1228);
        }));
}

function reverse_465(l_464) {
    return $effekt.withRegion(((this_929) =>
        {
            const tmp923_1235 = Nil_510();
            
            const res_523 = this_929.fresh(tmp923_1235);
            
            foreach_454(l_464, ((el_524) =>
                {
                    const tmp924_1234 = res_523.get_875();
                    
                    const tmp_1233 = res_523.put_876(Cons_511(el_524, tmp924_1234));
                    
                    return $effekt.pure(tmp_1233);
                })).run();
            
            const tmp_1232 = res_523.get_875();
            
            return $effekt.pure(tmp_1232);
        }));
}

function reverseOnto_469(l_467, other_468) {
    const tmp932_1236 = l_467;
    
    function tmp933_1237() {
        return $effekt.pure(other_468);
    }
    
    function tmp936_1238(a_525, rest_526) {
        return reverseOnto_469(rest_526, Cons_511(a_525, other_468));
    }
    
    switch (tmp932_1236.__tag) {
        case 0: return (() => { return tmp933_1237(); }).apply(null, tmp932_1236.__data);
        
        case 1: return ((tmp937_940, tmp938_939) =>
            {
                return tmp936_1238(tmp937_940, tmp938_939);
            }).apply(null, tmp932_1236.__data);
    }
    
    return $effekt.pure(null);
}

function append_473(l_471, other_472) {
    const tmp943_1239 = reverse_465(l_471).run();
    
    return reverseOnto_469(tmp943_1239, other_472);
}

function take_477(l_475, n_476) {
    return ((infixEq_82(n_476, 0)) ? $effekt.pure(Nil_510()) : (() =>
        {
            const tmp946_1240 = l_475;
            
            function tmp947_1241() {
                return $effekt.pure(Nil_510());
            }
            
            function tmp949_1242(a_527, rest_528) {
                const tmp948_1243 = take_477(rest_528, infixSub_36(n_476, 1)).run();
                
                return $effekt.pure(Cons_511(a_527, tmp948_1243));
            }
            
            switch (tmp946_1240.__tag) {
                case 0: return (() => { return tmp947_1241(); }).apply(null, tmp946_1240.__data);
                
                case 1: return ((tmp950_953, tmp951_952) =>
                    {
                        return tmp949_1242(tmp950_953, tmp951_952);
                    }).apply(null, tmp946_1240.__data);
            }
            
            return $effekt.pure(null);
        })());
}

function drop_481(l_479, n_480) {
    return ((infixEq_82(n_480, 0)) ? $effekt.pure(l_479) : (() =>
        {
            const tmp958_1244 = l_479;
            
            function tmp959_1245() {
                return $effekt.pure(Nil_510());
            }
            
            function tmp962_1246(a_529, rest_530) {
                return drop_481(rest_530, infixSub_36(n_480, 1));
            }
            
            switch (tmp958_1244.__tag) {
                case 0: return (() => { return tmp959_1245(); }).apply(null, tmp958_1244.__data);
                
                case 1: return ((tmp963_966, tmp964_965) =>
                    {
                        return tmp962_1246(tmp963_966, tmp964_965);
                    }).apply(null, tmp958_1244.__data);
            }
            
            return $effekt.pure(null);
        })());
}

function isEmpty_484(l_483) {
    const tmp971_1247 = l_483;
    
    function tmp972_1248() {
        return $effekt.pure(true);
    }
    
    function tmp973_1249(a_531, rest_532) {
        return $effekt.pure(false);
    }
    
    switch (tmp971_1247.__tag) {
        case 0: return (() => { return tmp972_1248(); }).apply(null, tmp971_1247.__data);
        
        case 1: return ((tmp974_977, tmp975_976) =>
            {
                return tmp973_1249(tmp974_977, tmp975_976);
            }).apply(null, tmp971_1247.__data);
    }
    
    return $effekt.pure(null);
}

function nonEmpty_487(l_486) {
    const tmp980_1250 = l_486;
    
    function tmp981_1251() {
        return $effekt.pure(false);
    }
    
    function tmp982_1252(a_533, rest_534) {
        return $effekt.pure(true);
    }
    
    switch (tmp980_1250.__tag) {
        case 0: return (() => { return tmp981_1251(); }).apply(null, tmp980_1250.__data);
        
        case 1: return ((tmp983_986, tmp984_985) =>
            {
                return tmp982_1252(tmp983_986, tmp984_985);
            }).apply(null, tmp980_1250.__data);
    }
    
    return $effekt.pure(null);
}

function head_490(l_489, Exception$capability_1256) {
    const tmp989_1253 = l_489;
    
    function tmp992_1254() {
        return Exception$capability_1256.raise_246(EmptyList_509(), "Trying to get the head of an empty list");
    }
    
    function tmp993_1255(a_535, rest_536) {
        return $effekt.pure(a_535);
    }
    
    switch (tmp989_1253.__tag) {
        case 0: return (() => { return tmp992_1254(); }).apply(null, tmp989_1253.__data);
        
        case 1: return ((tmp994_997, tmp995_996) =>
            {
                return tmp993_1255(tmp994_997, tmp995_996);
            }).apply(null, tmp989_1253.__data);
    }
    
    return $effekt.pure(null);
}

function tail_493(l_492, Exception$capability_1260) {
    const tmp1000_1257 = l_492;
    
    function tmp1003_1258() {
        return Exception$capability_1260.raise_246(EmptyList_509(), "Trying to get the head of an empty list");
    }
    
    function tmp1004_1259(a_537, rest_538) {
        return $effekt.pure(rest_538);
    }
    
    switch (tmp1000_1257.__tag) {
        case 0: return (() => { return tmp1003_1258(); }).apply(null, tmp1000_1257.__data);
        
        case 1: return ((tmp1005_1008, tmp1006_1007) =>
            {
                return tmp1004_1259(tmp1005_1008, tmp1006_1007);
            }).apply(null, tmp1000_1257.__data);
    }
    
    return $effekt.pure(null);
}

function headOption_496(l_495) {
    const tmp1011_1261 = l_495;
    
    function tmp1012_1262() {
        return $effekt.pure(None_384());
    }
    
    function tmp1013_1263(a_539, rest_540) {
        return $effekt.pure(Some_385(a_539));
    }
    
    switch (tmp1011_1261.__tag) {
        case 0: return (() => { return tmp1012_1262(); }).apply(null, tmp1011_1261.__data);
        
        case 1: return ((tmp1014_1017, tmp1015_1016) =>
            {
                return tmp1013_1263(tmp1014_1017, tmp1015_1016);
            }).apply(null, tmp1011_1261.__data);
    }
    
    return $effekt.pure(null);
}

function partition_500(l_498, pred_499) {
    return $effekt.withRegion(((this_1037) =>
        {
            const tmp1020_1274 = Nil_510();
            
            const lefts_541 = this_1037.fresh(tmp1020_1274);
            
            const tmp1021_1273 = Nil_510();
            
            const rights_542 = this_1037.fresh(tmp1021_1273);
            
            return foreach_454(l_498, ((el_543) =>
                {
                    return pred_499(el_543).then(((tmp1022_1264) =>
                        ((tmp1022_1264) ? (() =>
                            {
                                const tmp1023_1266 = lefts_541.get_875();
                                
                                const tmp_1265 = lefts_541.put_876(Cons_511(el_543, tmp1023_1266));
                                
                                return $effekt.pure(tmp_1265);
                            })() : (() =>
                            {
                                const tmp1026_1268 = rights_542.get_875();
                                
                                const tmp_1267 = rights_542.put_876(Cons_511(el_543, tmp1026_1268));
                                
                                return $effekt.pure(tmp_1267);
                            })())));
                })).then((() =>
                (() =>
                    {
                        const tmp1033_1272 = lefts_541.get_875();
                        
                        const tmp1034_1269 = reverse_465(tmp1033_1272).run();
                        
                        const tmp1035_1271 = rights_542.get_875();
                        
                        const tmp1036_1270 = reverse_465(tmp1035_1271).run();
                        
                        return $effekt.pure(Tuple2_198(tmp1034_1269, tmp1036_1270));
                    })()));
        }));
}

function sortBy_504(l_502, compare_503) {
    const tmp1040_1275 = l_502;
    
    function tmp1041_1276() {
        return $effekt.pure(Nil_510());
    }
    
    function tmp1063_1277(pivot_544, rest_545) {
        return partition_500(rest_545, ((el_546) =>
            {
                return compare_503(el_546, pivot_544);
            })).then(((tmp1044_1278) =>
            (() =>
                {
                    const tmp1045_1279 = tmp1044_1278;
                    
                    function tmp1056_1280(lt_547, gt_548) {
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
                    
                    switch (tmp1045_1279.__tag) {
                        case 0: return ((tmp1057_1060, tmp1058_1059) =>
                            {
                                return tmp1056_1280(tmp1057_1060, tmp1058_1059);
                            }).apply(null, tmp1045_1279.__data);
                    }
                    
                    return $effekt.pure(null);
                })()));
    }
    
    switch (tmp1040_1275.__tag) {
        case 0: return (() => { return tmp1041_1276(); }).apply(null, tmp1040_1275.__data);
        
        case 1: return ((tmp1064_1067, tmp1065_1066) =>
            {
                return tmp1063_1277(tmp1064_1067, tmp1065_1066);
            }).apply(null, tmp1040_1275.__data);
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

function iter_732(l_731, Yield$capability_1287) {
    return $effekt.withRegion(((this_852) =>
        {
            return (() =>
                {
                    const tmp809_1281 = l_731;
                    
                    function tmp810_1282() {
                        return $effekt.pure(42);
                    }
                    
                    function tmp811_1283(a_739, rest_740) {
                        return $effekt.pure(a_739);
                    }
                    
                    switch (tmp809_1281.__tag) {
                        case 0: return (() => { return tmp810_1282(); }).apply(null, tmp809_1281.__data);
                        
                        case 1: return ((tmp812_815, tmp813_814) =>
                            {
                                return tmp811_1283(tmp812_815, tmp813_814);
                            }).apply(null, tmp809_1281.__data);
                    }
                    
                    return $effekt.pure(null);
                })().then(((tmp818_1284) =>
                (() =>
                    {
                        const hd_741 = this_852.fresh(tmp818_1284);
                        
                        const tl_744 = (() =>
                            {
                                const tmp819_1296 = l_731;
                                
                                function tmp820_1297() {
                                    return $effekt.pure(Nil_510());
                                }
                                
                                function tmp821_1298(a_742, rest_743) {
                                    return $effekt.pure(rest_743);
                                }
                                
                                switch (tmp819_1296.__tag) {
                                    case 0: return (() =>
                                        {
                                            return tmp820_1297();
                                        }).apply(null, tmp819_1296.__data);
                                    
                                    case 1: return ((tmp822_825, tmp823_824) =>
                                        {
                                            return tmp821_1298(tmp822_825, tmp823_824);
                                        }).apply(null, tmp819_1296.__data);
                                }
                                
                                return $effekt.pure(null);
                            })().run();
                        
                        const tmp828_1295 = false;
                        
                        const toBehead_745 = this_852.fresh(tmp828_1295);
                        
                        return $effekt.handle([{
                            Replace_737: ((n_746, resume_747) =>
                                {
                                    const tmp834_1285 = hd_741.put_876(n_746);
                                    
                                    tmp834_1285;
                                    
                                    return resume_747($effekt.unit);
                                })
                        }, {
                            Behead_738: ((resume_748) =>
                                {
                                    const tmp837_1286 = toBehead_745.put_876(true);
                                    
                                    tmp837_1286;
                                    
                                    return resume_748($effekt.unit);
                                })
                        }])(((Replace$capability_1289, Behead$capability_1290) =>
                            {
                                const tmp829_1288 = hd_741.get_875();
                                
                                return Yield$capability_1287.Yield_735(tmp829_1288, Replace$capability_1289, Behead$capability_1290);
                            })).then((() =>
                            (() =>
                                {
                                    const tmp842_1291 = isEmpty_484(tl_744).run();
                                    
                                    return ((tmp842_1291) ? (() =>
                                        {
                                            const tmp843_1292 = hd_741.get_875();
                                            
                                            return $effekt.pure(Cons_511(tmp843_1292, Nil_510()));
                                        })() : iter_732(tl_744, Yield$capability_1287).then(((newTail_749) =>
                                        (() =>
                                            {
                                                const tmp846_1293 = toBehead_745.get_875();
                                                
                                                return ((tmp846_1293) ? $effekt.pure(newTail_749) : (() =>
                                                    {
                                                        const tmp847_1294 = hd_741.get_875();
                                                        
                                                        return $effekt.pure(Cons_511(tmp847_1294, newTail_749));
                                                    })());
                                            })())));
                                })()));
                    })()));
        }));
}

function main_733() {
    return ignoring_179(((Exception$capability_1305) =>
        {
            return $effekt.withRegion(((this_870) =>
                {
                    const tmp855_1304 = Cons_511(0, Cons_511(1, Cons_511(3, Cons_511(infixSub_36(0, 2), Cons_511(infixSub_36(0, 8), Cons_511(9, Nil_510()))))));
                    
                    const lst_750 = this_870.fresh(tmp855_1304);
                    
                    const res_753 = $effekt.handle([{
                        Yield_735: ((x_751, resume_752) =>
                            {
                                return resume_752(((Replace$capability_1301, Behead$capability_1300) =>
                                    {
                                        return ((infixLt_89(x_751, 0)) ? Behead$capability_1300.Behead_738() : Replace$capability_1301.Replace_737(infixMul_30(x_751, 2)));
                                    }));
                            })
                    }])(((Yield$capability_1303) =>
                        {
                            const tmp856_1302 = lst_750.get_875();
                            
                            return iter_732(tmp856_1302, Yield$capability_1303);
                        })).run();
                    
                    const tmp869_1299 = println_6(res_753);
                    
                    return $effekt.pure(tmp869_1299);
                }));
        }));
}

module.exports = { main: main_733 };