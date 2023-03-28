datatype ('arg1216, 'arg1217) Tuple2_137 =
  Tuple2_197 of ('arg1216 * 'arg1217);


fun first_200 (Tuple2_197 (first_200, _)) = first_200;


fun second_201 (Tuple2_197 (_, second_201)) = second_201;


datatype ('arg1218, 'arg1219, 'arg1220) Tuple3_141 =
  Tuple3_202 of ('arg1218 * 'arg1219 * 'arg1220);


fun firstDollar1_206 (Tuple3_202 (firstDollar1_206, _, _)) =
  firstDollar1_206;


fun secondDollar1_207 (Tuple3_202 (_, secondDollar1_207, _)) =
  secondDollar1_207;


fun third_208 (Tuple3_202 (_, _, third_208)) = third_208;


datatype ('arg1221, 'arg1222, 'arg1223, 'arg1224) Tuple4_146 =
  Tuple4_209 of ('arg1221 * 'arg1222 * 'arg1223 * 'arg1224);


fun firstDollar2_214 (Tuple4_209 (firstDollar2_214, _, _, _)) =
  firstDollar2_214;


fun secondDollar2_215 (Tuple4_209 (_, secondDollar2_215, _, _)) =
  secondDollar2_215;


fun thirdDollar1_216 (Tuple4_209 (_, _, thirdDollar1_216, _)) =
  thirdDollar1_216;


fun fourth_217 (Tuple4_209 (_, _, _, fourth_217)) = fourth_217;


datatype ('arg1225, 'arg1226, 'arg1227, 'arg1228, 'arg1229) Tuple5_152 =
  Tuple5_218 of ('arg1225 * 'arg1226 * 'arg1227 * 'arg1228 * 'arg1229);


fun firstDollar3_224 (Tuple5_218 (firstDollar3_224, _, _, _, _)) =
  firstDollar3_224;


fun secondDollar3_225 (Tuple5_218 (_, secondDollar3_225, _, _, _)) =
  secondDollar3_225;


fun thirdDollar2_226 (Tuple5_218 (_, _, thirdDollar2_226, _, _)) =
  thirdDollar2_226;


fun fourthDollar1_227 (Tuple5_218 (_, _, _, fourthDollar1_227, _)) =
  fourthDollar1_227;


fun fifth_228 (Tuple5_218 (_, _, _, _, fifth_228)) = fifth_228;


datatype ('arg1230, 'arg1231, 'arg1232, 'arg1233, 'arg1234, 'arg1235) Tuple6_159 =
  Tuple6_229 of ('arg1230 * 'arg1231 * 'arg1232 * 'arg1233 * 'arg1234 * 'arg1235);


fun firstDollar4_236 (Tuple6_229 (firstDollar4_236, _, _, _, _, _)) =
  firstDollar4_236;


fun secondDollar4_237 (Tuple6_229 (_, secondDollar4_237, _, _, _, _)) =
  secondDollar4_237;


fun thirdDollar3_238 (Tuple6_229 (_, _, thirdDollar3_238, _, _, _)) =
  thirdDollar3_238;


fun fourthDollar2_239 (Tuple6_229 (_, _, _, fourthDollar2_239, _, _)) =
  fourthDollar2_239;


fun fifthDollar1_240 (Tuple6_229 (_, _, _, _, fifthDollar1_240, _)) =
  fifthDollar1_240;


fun sixth_241 (Tuple6_229 (_, _, _, _, _, sixth_241)) = sixth_241;


datatype 'arg1236 Exception_164 =
  Exception_164 of 'arg1236;


fun raise_244 (Exception_164 raise_244) = raise_244;


datatype  RuntimeError_165 =
  RuntimeError_245;


datatype 'A_350 Option_349 =
  None_378
  | Some_379 of 'A_350;


datatype  EmptyList_460 =
  EmptyList_516;


datatype 'A_459 List_461 =
  Nil_517
  | Cons_518 of ('A_459 * ('A_459 List_461));


datatype 'arg1237 Exc_737 =
  Exc_737 of 'arg1237;


fun Exc_740 (Exc_737 Exc_740) = Exc_740;


(* EVIDENCE *)
fun lift m k1 k2 = m (fn a => k1 a k2);
fun nested ev1 ev2 m = ev1 (ev2 m);
fun here x = x;

(* REGIONS *)
(* type region = ((() -> () -> ()) list) ref *)
fun newRegion () = ref nil
fun backup cell = fn () => let val oldState = !cell in fn () => cell := oldState end
fun fresh r init = let val cell = ref init in (r := (backup cell) :: (!r); cell) end
fun withRegion body =
    let val r = newRegion()
        fun lift m k = let val fields = map (fn f => f()) (!r) in
            m (fn a => (map (fn f => f()) fields; k a)) end
    in body lift r end

(* SHOW INSTANCES *)
fun show'int x =
    let val s = (Int.toString x) in
    case String.sub (s, 0) of
          #"~" => "-" ^ String.extract (s, 1, NONE)
        | _ => s
    end;

(*
To align with the js backend inf and nan is rewritten
- `nan` -> `NaN`
- `inf` -> `Infinity`
- `~2.1` -> `-2.1`
They do still disagree on the truncating of `2.0` to `2` (ml does not).
*)
fun show'real x =
    let val s = (Real.toString x) in
    case s of
          "nan" => "NaN"
        | "inf" => "Infinity"
        | _ => case String.sub (s, 0) of
              #"~" => "-" ^ String.extract (s, 1, NONE)
            | _ => s
    end;

fun show'unit x = ""

fun show'bool x = Bool.toString x

fun show'string x = x

(* TIMING *)
val mlStartTime = Time.toMilliseconds (Time.now ());

(* RANDOM *)
fun mlRandomReal () =
   let
      val r = MLton.Random.rand ();
      val rreal = MLton.Real.fromWord r;
      val wsize = Real.fromInt Word.wordSize;
      fun shiftRight r shift = r / (Math.pow(2.0, shift))
   in
      shiftRight rreal wsize
   end;

(* HOLES *)
exception Hole



fun println_5 value = (print ((show'int value) ^ "\n"));


fun printlnDollar1_7 value = (print ((show'unit value) ^ "\n"));


fun printlnDollar2_9 value = (print ((show'real value) ^ "\n"));


fun printlnDollar3_11 value = (print ((show'string value) ^ "\n"));


fun printlnDollar4_13 value = (print ((show'bool value) ^ "\n"));


fun random_14 () = (mlRandomReal ());


fun timestamp_15 () =
  (IntInf.toInt ((Time.toMilliseconds (Time.now ())) - mlStartTime));


fun show_17 value = (show'int value);


fun showDollar1_19 value = (show'unit value);


fun showDollar2_21 value = (show'real value);


fun showDollar3_23 value = (show'string value);


fun showDollar4_25 value = (show'bool value);


fun infixConcat_28 s1 s2 = (s1 ^ s2);


fun infixAdd_31 x y = ((x: int) + y);


fun infixMul_34 x y = ((x: int) * y);


fun infixDiv_37 x y = ((x: int) div y);


fun infixSub_40 x y = ((x: int) - y);


fun mod_43 x y = ((x: int) mod y);


fun infixAddDollar1_46 x y = ((x: real) + y);


fun infixMulDollar1_49 x y = ((x: real) * y);


fun infixSubDollar1_52 x y = ((x: real) - y);


fun infixDivDollar1_55 x y = ((x: real) / y);


fun cos_57 x = (Math.cos x);


fun sin_59 x = (Math.sin x);


fun atan_61 x = (Math.atan x);


fun tan_63 x = (Math.tan x);


fun sqrt_65 x = (Math.sqrt x);


fun square_67 x = ((x: real) * x);


fun log_69 x = (Math.ln x);


fun log1p_71 x = (Math.ln (x + 1.0));


fun exp_73 x = (Math.exp x);


fun f_pi_74 () = (Math.pi);


fun toInt_76 d = (Real.round d);


fun toDouble_78 d = (Real.fromInt d);


fun floor_80 d = (Real.floor d);


fun ceil_82 d = (Real.ceil d);


fun infixEq_86 x y = (x = y);


fun infixNeq_90 x y = (not (x = y));


fun infixLt_93 x y = ((x: int) < y);


fun infixLte_96 x y = ((x: int) <= y);


fun infixGt_99 x y = ((x: int) > y);


fun infixGte_102 x y = ((x: int) >= y);


fun infixLtDollar1_105 x y = ((x: real) < y);


fun infixLteDollar1_108 x y = ((x: real) <= y);


fun infixGtDollar1_111 x y = ((x: real) > y);


fun infixGteDollar1_114 x y = ((x: real) >= y);


fun infixLtDollar2_117 x y = ((x: string) < y);


fun infixLteDollar2_120 x y = ((x: string) <= y);


fun infixGtDollar2_123 x y = ((x: string) > y);


fun infixGteDollar2_126 x y = ((x: string) >= y);


fun not_128 b = (not b);


fun infixOr_131 x y = (x orelse y);


fun infixAnd_134 x y = (x andalso y);


fun panic_162 msg = (raise Fail msg);


fun isDefined_434 opt = (Option.isSome opt);


fun force_440 opt = (Option.valOf opt);


fun length_703 str = (String.size str);


fun startsWith_706 str prefix = (String.isPrefix prefix str);


fun endsWith_709 str prefix = (String.isSuffix prefix str);


fun unsafeSubstring_715 str from = (String.extract (str, from, NONE));


fun internalStringToInt_717 str = (Int.fromString str);


fun unsafeCharAt_722 str n = (String.sub (str, n));


fun locally_3 ev1053_1053 ev1054_1054 f_1 k1238 = (f_1 ev1054_1054 k1238);


val PI_196 = (f_pi_74 ());


fun raise_168 ev1055_1055 ev1056_1056 msg_167 ExceptionDollarcapability_1057 k1239 =
  ((raise_244 ExceptionDollarcapability_1057)
    ev1056_1056
    RuntimeError_245
    msg_167
    (fn a1242 =>
      let
        val tmp1003_1243 = a1242;
        val tmp1004_1244 = tmp1003_1243;
      in (raise Hole)
      end));


fun raiseDollar1_173 ev1059_1059 ev1060_1060 exception_171 msg_172 ExceptionDollarcapability_1061 k1245 =
  ((raise_244 ExceptionDollarcapability_1061)
    ev1060_1060
    exception_171
    msg_172
    (fn a1246 =>
      let
        val tmp1007_1247 = a1246;
        val tmp1008_1248 = tmp1007_1247;
      in (raise Hole)
      end));


fun panicOn_176 ev1062_1062 ev1063_1063 prog_175 k1249 =
  ((((fn ev1065_1065 => fn ExceptionDollarcapability_1066 => fn k1256 =>
          (prog_175
            (nested ev1065_1065 ev1063_1063)
            here
            ExceptionDollarcapability_1066
            k1256))
        lift
        (Exception_164 (fn ev1064_1064 => fn exception_246 => fn msg_247 => fn k1250 =>
          ((ev1064_1064
              (fn k1251 =>
                let fun resume_248 ev1253 a1252 = (ev1253 (k1251 a1252));
                in (fn k1254 =>
                  let val tmp1013_1255 = (panic_162 msg_247);
                  in (k1254 tmp1013_1255)
                  end)
                end))
            k1250))))
      (fn a1257 =>
        (fn k21258 =>
          (k21258 a1257))))
    k1249);


fun report_179 ev1067_1067 ev1068_1068 prog_178 k1259 =
  ((((fn ev1070_1070 => fn ExceptionDollarcapability_1071 => fn k1266 =>
          (prog_178
            (nested ev1070_1070 ev1068_1068)
            here
            ExceptionDollarcapability_1071
            k1266))
        lift
        (Exception_164 (fn ev1069_1069 => fn exception_249 => fn msg_250 => fn k1260 =>
          ((ev1069_1069
              (fn k1261 =>
                let fun resume_251 ev1263 a1262 = (ev1263 (k1261 a1262));
                in (fn k1264 =>
                  let val tmp1018_1265 = (printlnDollar3_11 msg_250);
                  in (k1264 tmp1018_1265)
                  end)
                end))
            k1260))))
      (fn a1267 =>
        (fn k21268 =>
          (k21268 a1267))))
    k1259);


fun ignoring_182 ev1072_1072 ev1073_1073 prog_181 k1269 =
  ((((fn ev1075_1075 => fn ExceptionDollarcapability_1076 => fn k1275 =>
          (prog_181
            (nested ev1075_1075 ev1073_1073)
            here
            ExceptionDollarcapability_1076
            k1275))
        lift
        (Exception_164 (fn ev1074_1074 => fn exception_252 => fn msg_253 => fn k1270 =>
          ((ev1074_1074
              (fn k1271 =>
                let fun resume_254 ev1273 a1272 = (ev1273 (k1271 a1272));
                in (fn k1274 => (k1274 ()))
                end))
            k1270))))
      (fn a1276 =>
        (fn k21277 =>
          (k21277 a1276))))
    k1269);


fun each_186 ev1077_1077 ev1078_1078 start_183 end_184 action_185 k1278 =
  let fun loop_256 ev1079_1079 i_255 k1279 =
    (if (infixLt_93 i_255 end_184) then (action_185
      (nested ev1079_1079 ev1078_1078)
      i_255
      (fn a1280 =>
        let val f__1281 = a1280;
        in (loop_256 ev1079_1079 (infixAdd_31 i_255 1) k1279)
        end)) else (k1279 ()));
  in (loop_256 here start_183 k1278)
  end;


fun repeat_189 ev1080_1080 ev1081_1081 n_187 action_188 k1282 =
  (each_186
    ev1080_1080
    here
    0
    n_187
    (fn ev1082_1082 => fn n_257 => fn k1283 =>
      (action_188 (nested ev1082_1082 ev1081_1081) k1283))
    k1282);


fun timed_191 ev1083_1083 ev1084_1084 block_190 k1284 =
  let val before_258 =
    let val tmp1037_1285 = (timestamp_15 ());
    in tmp1037_1285
    end;
  in (block_190
    ev1084_1084
    (fn a1286 =>
      let
        val f__1287 = a1286;
        val after_259 =
          let val tmp1040_1288 = (timestamp_15 ());
          in tmp1040_1288
          end;
      in (k1284 (infixMul_34 1000 (infixSub_40 after_259 before_258)))
      end))
  end;


fun measure_195 ev1085_1085 ev1086_1086 warmup_192 iterations_193 block_194 k1289 =
  let
    fun run_262 ev1087_1087 n_260 report_261 k1290 =
      (if (infixLte_96 n_260 0) then (k1290 ()) else (timed_191
        (nested ev1087_1087 ev1085_1085)
        here
        (fn ev1088_1088 => fn k1291 =>
          (block_194
            (nested (nested ev1088_1088 ev1087_1087) ev1086_1086)
            k1291))
        (fn a1292 =>
          let
            val time_263 = a1292;
            val k1293 =
              (fn a1294 =>
                let
                  val tmp1046_1295 = a1294;
                  val _ = tmp1046_1295;
                in
                  (run_262
                    ev1087_1087
                    (infixSub_40 n_260 1)
                    report_261
                    k1290)
                end);
          in
            (if report_261 then let val tmp1045_1296 =
              (println_5 time_263);
            in (k1293 tmp1045_1296)
            end else (k1293 ()))
          end)));
    val _ = (run_262 here warmup_192 false (fn a1297 => a1297));
  in (run_262 here iterations_193 true k1289)
  end;


fun isDefined_353 ev1089_1089 self_352 k1298 =
  let
    val tmp956_1299 = self_352;
    fun tmp957_1090 ev1092_1092 k1300 = (k1300 false);
    fun tmp958_1091 ev1093_1093 v_382 k1301 = (k1301 true);
  in
    (
      case tmp956_1299 of 
        None_378 => (tmp957_1090 here k1298)
        | Some_379 tmp959_1302 => (tmp958_1091 here tmp959_1302 k1298)
      )
  end;


fun isEmpty_356 ev1094_1094 self_355 k1303 =
  let val tmp962_1304 =
    (isDefined_353 ev1094_1094 self_355 (fn a1305 => a1305));
  in (k1303 (not_128 tmp962_1304))
  end;


fun orElse_360 ev1095_1095 ev1096_1096 self_358 that_359 k1306 =
  let
    val tmp963_1307 = self_358;
    fun tmp966_1097 ev1099_1099 k1308 =
      (that_359 (nested ev1099_1099 ev1096_1096) k1308);
    fun tmp967_1098 ev1100_1100 v_383 k1309 = (k1309 (Some_379 v_383));
  in
    (
      case tmp963_1307 of 
        None_378 => (tmp966_1097 here k1306)
        | Some_379 tmp968_1310 => (tmp967_1098 here tmp968_1310 k1306)
      )
  end;


fun getOrElse_364 ev1101_1101 ev1102_1102 self_362 that_363 k1311 =
  let
    val tmp971_1312 = self_362;
    fun tmp974_1103 ev1105_1105 k1313 =
      (that_363 (nested ev1105_1105 ev1102_1102) k1313);
    fun tmp975_1104 ev1106_1106 v_384 k1314 = (k1314 v_384);
  in
    (
      case tmp971_1312 of 
        None_378 => (tmp974_1103 here k1311)
        | Some_379 tmp976_1315 => (tmp975_1104 here tmp976_1315 k1311)
      )
  end;


fun map_369 ev1107_1107 ev1108_1108 self_367 f_368 k1316 =
  let
    val tmp979_1317 = self_367;
    fun tmp980_1109 ev1111_1111 k1318 = (k1318 None_378);
    fun tmp982_1110 ev1112_1112 v_385 k1319 =
      (f_368
        (nested ev1112_1112 ev1108_1108)
        v_385
        (fn a1320 =>
          let val tmp981_1321 = a1320;
          in (k1319 (Some_379 tmp981_1321))
          end));
  in
    (
      case tmp979_1317 of 
        None_378 => (tmp980_1109 here k1316)
        | Some_379 tmp983_1322 => (tmp982_1110 here tmp983_1322 k1316)
      )
  end;


fun foreach_373 ev1113_1113 ev1114_1114 self_371 f_372 k1323 =
  let
    val tmp986_1324 = self_371;
    fun tmp987_1115 ev1117_1117 k1325 = (k1325 ());
    fun tmp990_1116 ev1118_1118 v_386 k1326 =
      (f_372 (nested ev1118_1118 ev1114_1114) v_386 k1326);
  in
    (
      case tmp986_1324 of 
        None_378 => (tmp987_1115 here k1323)
        | Some_379 tmp991_1327 => (tmp990_1116 here tmp991_1327 k1323)
      )
  end;


fun show_377 ev1119_1119 ev1120_1120 o_375 showA_376 k1328 =
  let
    val tmp994_1329 = o_375;
    fun tmp995_1121 ev1123_1123 k1330 = (k1330 "None()");
    fun tmp997_1122 ev1124_1124 v_387 k1331 =
      (showA_376
        (nested ev1124_1124 ev1120_1120)
        v_387
        (fn a1332 =>
          let val tmp996_1333 = a1332;
          in (k1331
            (infixConcat_28 (infixConcat_28 "Some(" tmp996_1333) ")"))
          end));
  in
    (
      case tmp994_1329 of 
        None_378 => (tmp995_1121 here k1328)
        | Some_379 tmp998_1334 => (tmp997_1122 here tmp998_1334 k1328)
      )
  end;


fun isEmpty_437 ev1125_1125 opt_436 k1335 =
  (k1335 (not_128 (isDefined_434 opt_436)));


fun toOption_443 ev1126_1126 opt_442 k1336 =
  (if (isDefined_434 opt_442) then (k1336 (Some_379 (force_440 opt_442))) else (k1336
    None_378));


fun isEmpty_464 ev1127_1127 l_463 k1337 =
  let
    val tmp775_1338 = l_463;
    fun tmp776_1128 ev1130_1130 k1339 = (k1339 true);
    fun tmp777_1129 ev1131_1131 a_523 rest_524 k1340 = (k1340 false);
  in
    (
      case tmp775_1338 of 
        Nil_517 => (tmp776_1128 here k1337)
        | Cons_518 (tmp778_781, tmp779_780) => (tmp777_1129
          here
          tmp778_781
          tmp779_780
          k1337)
      )
  end;


fun foreach_468 ev1132_1132 ev1133_1133 l_466 f_467 k1341 =
  let fun loop_526 ev1134_1134 remainder_525 k1342 =
    let val tmp784_1343 =
      (isEmpty_464
        (nested ev1134_1134 ev1132_1132)
        remainder_525
        (fn a1344 =>
          a1344));
    in (if (not_128 tmp784_1343) then let
      val tmp785_1345 = remainder_525;
      fun tmp786_1135 ev1137_1137 k1346 = (k1346 ());
      fun tmp791_1136 ev1138_1138 a_527 as_528 k1347 =
        (f_467
          (nested (nested ev1138_1138 ev1134_1134) ev1133_1133)
          a_527
          (fn a1348 =>
            let val f__1349 = a1348;
            in (loop_526 (nested ev1138_1138 ev1134_1134) as_528 k1347)
            end));
    in
      (
        case tmp785_1345 of 
          Nil_517 => (tmp786_1135 here k1342)
          | Cons_518 (tmp792_795, tmp793_794) => (tmp791_1136
            here
            tmp792_795
            tmp793_794
            k1342)
        )
    end else (k1342 ()))
    end;
  in (loop_526 here l_466 k1341)
  end;


fun size_471 ev1139_1139 l_470 k1350 =
  let fun loop_531 ev1140_1140 lst_529 acc_530 k1351 =
    let
      val tmp802_1352 = lst_529;
      fun tmp803_1141 ev1143_1143 k1353 = (k1353 acc_530);
      fun tmp806_1142 ev1144_1144 a_532 as_533 k1354 =
        (loop_531
          (nested ev1144_1144 ev1140_1140)
          as_533
          (infixAdd_31 acc_530 1)
          k1354);
    in
      (
        case tmp802_1352 of 
          Nil_517 => (tmp803_1141 here k1351)
          | Cons_518 (tmp807_810, tmp808_809) => (tmp806_1142
            here
            tmp807_810
            tmp808_809
            k1351)
        )
    end;
  in (loop_531 here l_470 0 k1350)
  end;


fun reverse_474 ev1145_1145 l_473 k1355 =
  let fun loop_536 ev1146_1146 lst_534 acc_535 k1356 =
    let
      val tmp815_1357 = lst_534;
      fun tmp816_1147 ev1149_1149 k1358 = (k1358 acc_535);
      fun tmp819_1148 ev1150_1150 a_537 as_538 k1359 =
        (loop_536
          (nested ev1150_1150 ev1146_1146)
          as_538
          (Cons_518 (a_537, acc_535))
          k1359);
    in
      (
        case tmp815_1357 of 
          Nil_517 => (tmp816_1147 here k1356)
          | Cons_518 (tmp820_823, tmp821_822) => (tmp819_1148
            here
            tmp820_823
            tmp821_822
            k1356)
        )
    end;
  in (loop_536 here l_473 Nil_517 k1355)
  end;


fun map_479 ev1151_1151 ev1152_1152 l_477 f_478 k1360 =
  let fun loop_541 ev1153_1153 lst_539 acc_540 k1361 =
    let
      val tmp828_1362 = lst_539;
      fun tmp829_1154 ev1156_1156 k1363 = (k1363 acc_540);
      fun tmp833_1155 ev1157_1157 a_542 as_543 k1364 =
        (f_478
          (nested (nested ev1157_1157 ev1153_1153) ev1152_1152)
          a_542
          (fn a1365 =>
            let val tmp830_1366 = a1365;
            in (loop_541
              (nested ev1157_1157 ev1153_1153)
              as_543
              (Cons_518 (tmp830_1366, acc_540))
              k1364)
            end));
    in
      (
        case tmp828_1362 of 
          Nil_517 => (tmp829_1154 here k1361)
          | Cons_518 (tmp834_837, tmp835_836) => (tmp833_1155
            here
            tmp834_837
            tmp835_836
            k1361)
        )
    end;
  in (loop_541
    here
    l_477
    Nil_517
    (fn a1367 =>
      let val tmp840_1368 = a1367;
      in (reverse_474 ev1151_1151 tmp840_1368 k1360)
      end))
  end;


fun reverseOnto_483 ev1158_1158 l_481 other_482 k1369 =
  let
    val tmp843_1370 = l_481;
    fun tmp844_1159 ev1161_1161 k1371 = (k1371 other_482);
    fun tmp847_1160 ev1162_1162 a_544 rest_545 k1372 =
      (reverseOnto_483
        (nested ev1162_1162 ev1158_1158)
        rest_545
        (Cons_518 (a_544, other_482))
        k1372);
  in
    (
      case tmp843_1370 of 
        Nil_517 => (tmp844_1159 here k1369)
        | Cons_518 (tmp848_851, tmp849_850) => (tmp847_1160
          here
          tmp848_851
          tmp849_850
          k1369)
      )
  end;


fun append_487 ev1163_1163 l_485 other_486 k1373 =
  let val tmp854_1374 =
    (reverse_474 ev1163_1163 l_485 (fn a1375 => a1375));
  in (reverseOnto_483 ev1163_1163 tmp854_1374 other_486 k1373)
  end;


fun take_491 ev1164_1164 l_489 n_490 k1376 =
  (if (infixEq_86 n_490 0) then (k1376 Nil_517) else let
    val tmp857_1377 = l_489;
    fun tmp858_1165 ev1167_1167 k1378 = (k1378 Nil_517);
    fun tmp860_1166 ev1168_1168 a_546 rest_547 k1379 =
      let val tmp859_1380 =
        (take_491
          (nested ev1168_1168 ev1164_1164)
          rest_547
          (infixSub_40 n_490 1)
          (fn a1381 =>
            a1381));
      in (k1379 (Cons_518 (a_546, tmp859_1380)))
      end;
  in
    (
      case tmp857_1377 of 
        Nil_517 => (tmp858_1165 here k1376)
        | Cons_518 (tmp861_864, tmp862_863) => (tmp860_1166
          here
          tmp861_864
          tmp862_863
          k1376)
      )
  end);


fun drop_495 ev1169_1169 l_493 n_494 k1382 =
  (if (infixEq_86 n_494 0) then (k1382 l_493) else let
    val tmp869_1383 = l_493;
    fun tmp870_1170 ev1172_1172 k1384 = (k1384 Nil_517);
    fun tmp873_1171 ev1173_1173 a_548 rest_549 k1385 =
      (drop_495
        (nested ev1173_1173 ev1169_1169)
        rest_549
        (infixSub_40 n_494 1)
        k1385);
  in
    (
      case tmp869_1383 of 
        Nil_517 => (tmp870_1170 here k1382)
        | Cons_518 (tmp874_877, tmp875_876) => (tmp873_1171
          here
          tmp874_877
          tmp875_876
          k1382)
      )
  end);


fun nonEmpty_498 ev1174_1174 l_497 k1386 =
  let
    val tmp882_1387 = l_497;
    fun tmp883_1175 ev1177_1177 k1388 = (k1388 false);
    fun tmp884_1176 ev1178_1178 a_550 rest_551 k1389 = (k1389 true);
  in
    (
      case tmp882_1387 of 
        Nil_517 => (tmp883_1175 here k1386)
        | Cons_518 (tmp885_888, tmp886_887) => (tmp884_1176
          here
          tmp885_888
          tmp886_887
          k1386)
      )
  end;


fun head_501 ev1179_1179 ev1180_1180 l_500 ExceptionDollarcapability_1181 k1390 =
  let
    val tmp891_1391 = l_500;
    fun tmp894_1182 ev1184_1184 k1392 =
      ((raise_244 ExceptionDollarcapability_1181)
        (nested ev1184_1184 ev1180_1180)
        EmptyList_516
        "Trying to get the head of an empty list"
        k1392);
    fun tmp895_1183 ev1185_1185 a_552 rest_553 k1393 = (k1393 a_552);
  in
    (
      case tmp891_1391 of 
        Nil_517 => (tmp894_1182 here k1390)
        | Cons_518 (tmp896_899, tmp897_898) => (tmp895_1183
          here
          tmp896_899
          tmp897_898
          k1390)
      )
  end;


fun tail_504 ev1186_1186 ev1187_1187 l_503 ExceptionDollarcapability_1188 k1394 =
  let
    val tmp902_1395 = l_503;
    fun tmp905_1189 ev1191_1191 k1396 =
      ((raise_244 ExceptionDollarcapability_1188)
        (nested ev1191_1191 ev1187_1187)
        EmptyList_516
        "Trying to get the head of an empty list"
        k1396);
    fun tmp906_1190 ev1192_1192 a_554 rest_555 k1397 = (k1397 rest_555);
  in
    (
      case tmp902_1395 of 
        Nil_517 => (tmp905_1189 here k1394)
        | Cons_518 (tmp907_910, tmp908_909) => (tmp906_1190
          here
          tmp907_910
          tmp908_909
          k1394)
      )
  end;


fun headOption_507 ev1193_1193 l_506 k1398 =
  let
    val tmp913_1399 = l_506;
    fun tmp914_1194 ev1196_1196 k1400 = (k1400 None_378);
    fun tmp915_1195 ev1197_1197 a_556 rest_557 k1401 =
      (k1401 (Some_379 a_556));
  in
    (
      case tmp913_1399 of 
        Nil_517 => (tmp914_1194 here k1398)
        | Cons_518 (tmp916_919, tmp917_918) => (tmp915_1195
          here
          tmp916_919
          tmp917_918
          k1398)
      )
  end;


fun partition_511 ev1198_1198 ev1199_1199 l_509 pred_510 k1402 =
  ((withRegion
      (fn ev1200_1200 => fn this_939 => fn k1403 =>
        let
          val tmp922_1404 = Nil_517;
          val lefts_558 = (fresh this_939 tmp922_1404);
          val tmp923_1406 = Nil_517;
          val rights_559 = (fresh this_939 tmp923_1406);
        in
          (foreach_468
            (nested ev1200_1200 ev1198_1198)
            here
            l_509
            (fn ev1201_1201 => fn el_560 => fn k1407 =>
              (pred_510
                (nested (nested ev1201_1201 ev1200_1200) ev1199_1199)
                el_560
                (fn a1408 =>
                  let val tmp924_1409 = a1408;
                  in (if tmp924_1409 then let val tmp925_1410 =
                    (!lefts_558);
                  in (k1407 (lefts_558 := (Cons_518 (el_560, tmp925_1410))))
                  end else let val tmp928_1411 = (!rights_559);
                  in (k1407
                    (rights_559 := (Cons_518 (el_560, tmp928_1411))))
                  end)
                  end)))
            (fn a1412 =>
              let
                val f__1413 = a1412;
                val tmp935_1414 = (!lefts_558);
                val tmp936_1415 =
                  (reverse_474
                    (nested ev1200_1200 ev1198_1198)
                    tmp935_1414
                    (fn a1416 =>
                      a1416));
                val tmp937_1417 = (!rights_559);
                val tmp938_1418 =
                  (reverse_474
                    (nested ev1200_1200 ev1198_1198)
                    tmp937_1417
                    (fn a1419 =>
                      a1419));
              in (k1403 (Tuple2_197 (tmp936_1415, tmp938_1418)))
              end))
        end))
    k1402);


fun show_515 ev1202_1202 ev1203_1203 l_513 showA_514 k1420 =
  let
    val tmp942_1421 = l_513;
    fun tmp943_1204 ev1206_1206 k1422 = (k1422 "Nil()");
    fun tmp947_1205 ev1207_1207 x_561 xs_562 k1423 =
      (showA_514
        (nested ev1207_1207 ev1203_1203)
        x_561
        (fn a1424 =>
          let val tmp944_1425 = a1424;
          in (show_515
            (nested ev1207_1207 ev1202_1202)
            (nested ev1207_1207 ev1203_1203)
            xs_562
            showA_514
            (fn a1426 =>
              let val tmp946_1427 = a1426;
              in (k1423
                (infixConcat_28
                  (infixConcat_28
                    (infixConcat_28
                      (infixConcat_28 "Cons(" tmp944_1425)
                      ", ")
                    tmp946_1427)
                  ")"))
              end))
          end));
  in
    (
      case tmp942_1421 of 
        Nil_517 => (tmp943_1204 here k1420)
        | Cons_518 (tmp948_951, tmp949_950) => (tmp947_1205
          here
          tmp948_951
          tmp949_950
          k1420)
      )
  end;


fun charAt_701 ev1208_1208 str_700 index_699 k1428 =
  (if (infixOr_131
    (infixLt_93 index_699 0)
    (infixLte_96 (length_703 str_700) index_699)) then (k1428
    (Some_379 (unsafeCharAt_722 str_700 index_699))) else (k1428 None_378));


fun substring_712 ev1209_1209 str_710 from_711 k1429 =
  (if (infixOr_131
    (infixLt_93 from_711 0)
    (infixLte_96 (length_703 str_710) from_711)) then (k1429 str_710) else (k1429
    (unsafeSubstring_715 str_710 from_711)));


fun toInt_719 ev1210_1210 str_718 k1430 =
  (toOption_443 ev1210_1210 (internalStringToInt_717 str_718) k1430);


fun run_738 ev1211_1211 n_736 k1431 =
  (if (infixEq_86 n_736 0) then ((((fn ev1213_1213 => fn ExcDollarcapability_1214 => fn k1437 =>
          ((Exc_740 ExcDollarcapability_1214) here k1437))
        lift
        (Exc_737 (fn ev1212_1212 => fn k1432 =>
          ((ev1212_1212
              (fn k1433 =>
                let fun resume_741 ev1435 a1434 = (ev1435 (k1433 a1434));
                in (fn k1436 => (resume_741 here 0 k1436))
                end))
            k1432))))
      (fn a1438 =>
        (fn k21439 =>
          (k21439 a1438))))
    k1431) else (run_738 ev1211_1211 (infixSub_40 n_736 1) k1431));


fun main_739 ev1215_1215 k1440 =
  let
    val tmp766_1441 = (run_738 ev1215_1215 100100100 (fn a1442 => a1442));
    val tmp768_1443 = (println_5 tmp766_1441);
  in (k1440 tmp768_1443)
  end;

(main_739 (fn a => a) (fn a => a));
