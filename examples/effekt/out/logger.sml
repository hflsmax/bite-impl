datatype ('arg1245, 'arg1246) Tuple2_137 =
  Tuple2_197 of ('arg1245 * 'arg1246);


fun first_200 (Tuple2_197 (first_200, _)) = first_200;


fun second_201 (Tuple2_197 (_, second_201)) = second_201;


datatype ('arg1247, 'arg1248, 'arg1249) Tuple3_141 =
  Tuple3_202 of ('arg1247 * 'arg1248 * 'arg1249);


fun firstDollar1_206 (Tuple3_202 (firstDollar1_206, _, _)) =
  firstDollar1_206;


fun secondDollar1_207 (Tuple3_202 (_, secondDollar1_207, _)) =
  secondDollar1_207;


fun third_208 (Tuple3_202 (_, _, third_208)) = third_208;


datatype ('arg1250, 'arg1251, 'arg1252, 'arg1253) Tuple4_146 =
  Tuple4_209 of ('arg1250 * 'arg1251 * 'arg1252 * 'arg1253);


fun firstDollar2_214 (Tuple4_209 (firstDollar2_214, _, _, _)) =
  firstDollar2_214;


fun secondDollar2_215 (Tuple4_209 (_, secondDollar2_215, _, _)) =
  secondDollar2_215;


fun thirdDollar1_216 (Tuple4_209 (_, _, thirdDollar1_216, _)) =
  thirdDollar1_216;


fun fourth_217 (Tuple4_209 (_, _, _, fourth_217)) = fourth_217;


datatype ('arg1254, 'arg1255, 'arg1256, 'arg1257, 'arg1258) Tuple5_152 =
  Tuple5_218 of ('arg1254 * 'arg1255 * 'arg1256 * 'arg1257 * 'arg1258);


fun firstDollar3_224 (Tuple5_218 (firstDollar3_224, _, _, _, _)) =
  firstDollar3_224;


fun secondDollar3_225 (Tuple5_218 (_, secondDollar3_225, _, _, _)) =
  secondDollar3_225;


fun thirdDollar2_226 (Tuple5_218 (_, _, thirdDollar2_226, _, _)) =
  thirdDollar2_226;


fun fourthDollar1_227 (Tuple5_218 (_, _, _, fourthDollar1_227, _)) =
  fourthDollar1_227;


fun fifth_228 (Tuple5_218 (_, _, _, _, fifth_228)) = fifth_228;


datatype ('arg1259, 'arg1260, 'arg1261, 'arg1262, 'arg1263, 'arg1264) Tuple6_159 =
  Tuple6_229 of ('arg1259 * 'arg1260 * 'arg1261 * 'arg1262 * 'arg1263 * 'arg1264);


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


datatype 'arg1265 Exception_164 =
  Exception_164 of 'arg1265;


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


datatype 'arg1266 LogInfo_737 =
  LogInfo_737 of 'arg1266;


fun LogInfo_744 (LogInfo_737 LogInfo_744) = LogInfo_744;


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


fun locally_3 ev1077_1077 ev1078_1078 f_1 k1267 = (f_1 ev1078_1078 k1267);


val PI_196 = (f_pi_74 ());


fun raise_168 ev1079_1079 ev1080_1080 msg_167 ExceptionDollarcapability_1081 k1268 =
  ((raise_244 ExceptionDollarcapability_1081)
    ev1080_1080
    RuntimeError_245
    msg_167
    (fn a1271 =>
      let
        val tmp1027_1272 = a1271;
        val tmp1028_1273 = tmp1027_1272;
      in (raise Hole)
      end));


fun raiseDollar1_173 ev1083_1083 ev1084_1084 exception_171 msg_172 ExceptionDollarcapability_1085 k1274 =
  ((raise_244 ExceptionDollarcapability_1085)
    ev1084_1084
    exception_171
    msg_172
    (fn a1275 =>
      let
        val tmp1031_1276 = a1275;
        val tmp1032_1277 = tmp1031_1276;
      in (raise Hole)
      end));


fun panicOn_176 ev1086_1086 ev1087_1087 prog_175 k1278 =
  ((((fn ev1089_1089 => fn ExceptionDollarcapability_1090 => fn k1285 =>
          (prog_175
            (nested ev1089_1089 ev1087_1087)
            here
            ExceptionDollarcapability_1090
            k1285))
        lift
        (Exception_164 (fn ev1088_1088 => fn exception_246 => fn msg_247 => fn k1279 =>
          ((ev1088_1088
              (fn k1280 =>
                let fun resume_248 ev1282 a1281 = (ev1282 (k1280 a1281));
                in (fn k1283 =>
                  let val tmp1037_1284 = (panic_162 msg_247);
                  in (k1283 tmp1037_1284)
                  end)
                end))
            k1279))))
      (fn a1286 =>
        (fn k21287 =>
          (k21287 a1286))))
    k1278);


fun report_179 ev1091_1091 ev1092_1092 prog_178 k1288 =
  ((((fn ev1094_1094 => fn ExceptionDollarcapability_1095 => fn k1295 =>
          (prog_178
            (nested ev1094_1094 ev1092_1092)
            here
            ExceptionDollarcapability_1095
            k1295))
        lift
        (Exception_164 (fn ev1093_1093 => fn exception_249 => fn msg_250 => fn k1289 =>
          ((ev1093_1093
              (fn k1290 =>
                let fun resume_251 ev1292 a1291 = (ev1292 (k1290 a1291));
                in (fn k1293 =>
                  let val tmp1042_1294 = (printlnDollar3_11 msg_250);
                  in (k1293 tmp1042_1294)
                  end)
                end))
            k1289))))
      (fn a1296 =>
        (fn k21297 =>
          (k21297 a1296))))
    k1288);


fun ignoring_182 ev1096_1096 ev1097_1097 prog_181 k1298 =
  ((((fn ev1099_1099 => fn ExceptionDollarcapability_1100 => fn k1304 =>
          (prog_181
            (nested ev1099_1099 ev1097_1097)
            here
            ExceptionDollarcapability_1100
            k1304))
        lift
        (Exception_164 (fn ev1098_1098 => fn exception_252 => fn msg_253 => fn k1299 =>
          ((ev1098_1098
              (fn k1300 =>
                let fun resume_254 ev1302 a1301 = (ev1302 (k1300 a1301));
                in (fn k1303 => (k1303 ()))
                end))
            k1299))))
      (fn a1305 =>
        (fn k21306 =>
          (k21306 a1305))))
    k1298);


fun each_186 ev1101_1101 ev1102_1102 start_183 end_184 action_185 k1307 =
  let fun loop_256 ev1103_1103 i_255 k1308 =
    (if (infixLt_93 i_255 end_184) then (action_185
      (nested ev1103_1103 ev1102_1102)
      i_255
      (fn a1309 =>
        let val f__1310 = a1309;
        in (loop_256 ev1103_1103 (infixAdd_31 i_255 1) k1308)
        end)) else (k1308 ()));
  in (loop_256 here start_183 k1307)
  end;


fun repeat_189 ev1104_1104 ev1105_1105 n_187 action_188 k1311 =
  (each_186
    ev1104_1104
    here
    0
    n_187
    (fn ev1106_1106 => fn n_257 => fn k1312 =>
      (action_188 (nested ev1106_1106 ev1105_1105) k1312))
    k1311);


fun timed_191 ev1107_1107 ev1108_1108 block_190 k1313 =
  let val before_258 =
    let val tmp1061_1314 = (timestamp_15 ());
    in tmp1061_1314
    end;
  in (block_190
    ev1108_1108
    (fn a1315 =>
      let
        val f__1316 = a1315;
        val after_259 =
          let val tmp1064_1317 = (timestamp_15 ());
          in tmp1064_1317
          end;
      in (k1313 (infixMul_34 1000 (infixSub_40 after_259 before_258)))
      end))
  end;


fun measure_195 ev1109_1109 ev1110_1110 warmup_192 iterations_193 block_194 k1318 =
  let
    fun run_262 ev1111_1111 n_260 report_261 k1319 =
      (if (infixLte_96 n_260 0) then (k1319 ()) else (timed_191
        (nested ev1111_1111 ev1109_1109)
        here
        (fn ev1112_1112 => fn k1320 =>
          (block_194
            (nested (nested ev1112_1112 ev1111_1111) ev1110_1110)
            k1320))
        (fn a1321 =>
          let
            val time_263 = a1321;
            val k1322 =
              (fn a1323 =>
                let
                  val tmp1070_1324 = a1323;
                  val _ = tmp1070_1324;
                in
                  (run_262
                    ev1111_1111
                    (infixSub_40 n_260 1)
                    report_261
                    k1319)
                end);
          in
            (if report_261 then let val tmp1069_1325 =
              (println_5 time_263);
            in (k1322 tmp1069_1325)
            end else (k1322 ()))
          end)));
    val _ = (run_262 here warmup_192 false (fn a1326 => a1326));
  in (run_262 here iterations_193 true k1318)
  end;


fun isDefined_353 ev1113_1113 self_352 k1327 =
  let
    val tmp980_1328 = self_352;
    fun tmp981_1114 ev1116_1116 k1329 = (k1329 false);
    fun tmp982_1115 ev1117_1117 v_382 k1330 = (k1330 true);
  in
    (
      case tmp980_1328 of 
        None_378 => (tmp981_1114 here k1327)
        | Some_379 tmp983_1331 => (tmp982_1115 here tmp983_1331 k1327)
      )
  end;


fun isEmpty_356 ev1118_1118 self_355 k1332 =
  let val tmp986_1333 =
    (isDefined_353 ev1118_1118 self_355 (fn a1334 => a1334));
  in (k1332 (not_128 tmp986_1333))
  end;


fun orElse_360 ev1119_1119 ev1120_1120 self_358 that_359 k1335 =
  let
    val tmp987_1336 = self_358;
    fun tmp990_1121 ev1123_1123 k1337 =
      (that_359 (nested ev1123_1123 ev1120_1120) k1337);
    fun tmp991_1122 ev1124_1124 v_383 k1338 = (k1338 (Some_379 v_383));
  in
    (
      case tmp987_1336 of 
        None_378 => (tmp990_1121 here k1335)
        | Some_379 tmp992_1339 => (tmp991_1122 here tmp992_1339 k1335)
      )
  end;


fun getOrElse_364 ev1125_1125 ev1126_1126 self_362 that_363 k1340 =
  let
    val tmp995_1341 = self_362;
    fun tmp998_1127 ev1129_1129 k1342 =
      (that_363 (nested ev1129_1129 ev1126_1126) k1342);
    fun tmp999_1128 ev1130_1130 v_384 k1343 = (k1343 v_384);
  in
    (
      case tmp995_1341 of 
        None_378 => (tmp998_1127 here k1340)
        | Some_379 tmp1000_1344 => (tmp999_1128 here tmp1000_1344 k1340)
      )
  end;


fun map_369 ev1131_1131 ev1132_1132 self_367 f_368 k1345 =
  let
    val tmp1003_1346 = self_367;
    fun tmp1004_1133 ev1135_1135 k1347 = (k1347 None_378);
    fun tmp1006_1134 ev1136_1136 v_385 k1348 =
      (f_368
        (nested ev1136_1136 ev1132_1132)
        v_385
        (fn a1349 =>
          let val tmp1005_1350 = a1349;
          in (k1348 (Some_379 tmp1005_1350))
          end));
  in
    (
      case tmp1003_1346 of 
        None_378 => (tmp1004_1133 here k1345)
        | Some_379 tmp1007_1351 => (tmp1006_1134 here tmp1007_1351 k1345)
      )
  end;


fun foreach_373 ev1137_1137 ev1138_1138 self_371 f_372 k1352 =
  let
    val tmp1010_1353 = self_371;
    fun tmp1011_1139 ev1141_1141 k1354 = (k1354 ());
    fun tmp1014_1140 ev1142_1142 v_386 k1355 =
      (f_372 (nested ev1142_1142 ev1138_1138) v_386 k1355);
  in
    (
      case tmp1010_1353 of 
        None_378 => (tmp1011_1139 here k1352)
        | Some_379 tmp1015_1356 => (tmp1014_1140 here tmp1015_1356 k1352)
      )
  end;


fun show_377 ev1143_1143 ev1144_1144 o_375 showA_376 k1357 =
  let
    val tmp1018_1358 = o_375;
    fun tmp1019_1145 ev1147_1147 k1359 = (k1359 "None()");
    fun tmp1021_1146 ev1148_1148 v_387 k1360 =
      (showA_376
        (nested ev1148_1148 ev1144_1144)
        v_387
        (fn a1361 =>
          let val tmp1020_1362 = a1361;
          in (k1360
            (infixConcat_28 (infixConcat_28 "Some(" tmp1020_1362) ")"))
          end));
  in
    (
      case tmp1018_1358 of 
        None_378 => (tmp1019_1145 here k1357)
        | Some_379 tmp1022_1363 => (tmp1021_1146 here tmp1022_1363 k1357)
      )
  end;


fun isEmpty_437 ev1149_1149 opt_436 k1364 =
  (k1364 (not_128 (isDefined_434 opt_436)));


fun toOption_443 ev1150_1150 opt_442 k1365 =
  (if (isDefined_434 opt_442) then (k1365 (Some_379 (force_440 opt_442))) else (k1365
    None_378));


fun isEmpty_464 ev1151_1151 l_463 k1366 =
  let
    val tmp799_1367 = l_463;
    fun tmp800_1152 ev1154_1154 k1368 = (k1368 true);
    fun tmp801_1153 ev1155_1155 a_523 rest_524 k1369 = (k1369 false);
  in
    (
      case tmp799_1367 of 
        Nil_517 => (tmp800_1152 here k1366)
        | Cons_518 (tmp802_805, tmp803_804) => (tmp801_1153
          here
          tmp802_805
          tmp803_804
          k1366)
      )
  end;


fun foreach_468 ev1156_1156 ev1157_1157 l_466 f_467 k1370 =
  let fun loop_526 ev1158_1158 remainder_525 k1371 =
    let val tmp808_1372 =
      (isEmpty_464
        (nested ev1158_1158 ev1156_1156)
        remainder_525
        (fn a1373 =>
          a1373));
    in (if (not_128 tmp808_1372) then let
      val tmp809_1374 = remainder_525;
      fun tmp810_1159 ev1161_1161 k1375 = (k1375 ());
      fun tmp815_1160 ev1162_1162 a_527 as_528 k1376 =
        (f_467
          (nested (nested ev1162_1162 ev1158_1158) ev1157_1157)
          a_527
          (fn a1377 =>
            let val f__1378 = a1377;
            in (loop_526 (nested ev1162_1162 ev1158_1158) as_528 k1376)
            end));
    in
      (
        case tmp809_1374 of 
          Nil_517 => (tmp810_1159 here k1371)
          | Cons_518 (tmp816_819, tmp817_818) => (tmp815_1160
            here
            tmp816_819
            tmp817_818
            k1371)
        )
    end else (k1371 ()))
    end;
  in (loop_526 here l_466 k1370)
  end;


fun size_471 ev1163_1163 l_470 k1379 =
  let fun loop_531 ev1164_1164 lst_529 acc_530 k1380 =
    let
      val tmp826_1381 = lst_529;
      fun tmp827_1165 ev1167_1167 k1382 = (k1382 acc_530);
      fun tmp830_1166 ev1168_1168 a_532 as_533 k1383 =
        (loop_531
          (nested ev1168_1168 ev1164_1164)
          as_533
          (infixAdd_31 acc_530 1)
          k1383);
    in
      (
        case tmp826_1381 of 
          Nil_517 => (tmp827_1165 here k1380)
          | Cons_518 (tmp831_834, tmp832_833) => (tmp830_1166
            here
            tmp831_834
            tmp832_833
            k1380)
        )
    end;
  in (loop_531 here l_470 0 k1379)
  end;


fun reverse_474 ev1169_1169 l_473 k1384 =
  let fun loop_536 ev1170_1170 lst_534 acc_535 k1385 =
    let
      val tmp839_1386 = lst_534;
      fun tmp840_1171 ev1173_1173 k1387 = (k1387 acc_535);
      fun tmp843_1172 ev1174_1174 a_537 as_538 k1388 =
        (loop_536
          (nested ev1174_1174 ev1170_1170)
          as_538
          (Cons_518 (a_537, acc_535))
          k1388);
    in
      (
        case tmp839_1386 of 
          Nil_517 => (tmp840_1171 here k1385)
          | Cons_518 (tmp844_847, tmp845_846) => (tmp843_1172
            here
            tmp844_847
            tmp845_846
            k1385)
        )
    end;
  in (loop_536 here l_473 Nil_517 k1384)
  end;


fun map_479 ev1175_1175 ev1176_1176 l_477 f_478 k1389 =
  let fun loop_541 ev1177_1177 lst_539 acc_540 k1390 =
    let
      val tmp852_1391 = lst_539;
      fun tmp853_1178 ev1180_1180 k1392 = (k1392 acc_540);
      fun tmp857_1179 ev1181_1181 a_542 as_543 k1393 =
        (f_478
          (nested (nested ev1181_1181 ev1177_1177) ev1176_1176)
          a_542
          (fn a1394 =>
            let val tmp854_1395 = a1394;
            in (loop_541
              (nested ev1181_1181 ev1177_1177)
              as_543
              (Cons_518 (tmp854_1395, acc_540))
              k1393)
            end));
    in
      (
        case tmp852_1391 of 
          Nil_517 => (tmp853_1178 here k1390)
          | Cons_518 (tmp858_861, tmp859_860) => (tmp857_1179
            here
            tmp858_861
            tmp859_860
            k1390)
        )
    end;
  in (loop_541
    here
    l_477
    Nil_517
    (fn a1396 =>
      let val tmp864_1397 = a1396;
      in (reverse_474 ev1175_1175 tmp864_1397 k1389)
      end))
  end;


fun reverseOnto_483 ev1182_1182 l_481 other_482 k1398 =
  let
    val tmp867_1399 = l_481;
    fun tmp868_1183 ev1185_1185 k1400 = (k1400 other_482);
    fun tmp871_1184 ev1186_1186 a_544 rest_545 k1401 =
      (reverseOnto_483
        (nested ev1186_1186 ev1182_1182)
        rest_545
        (Cons_518 (a_544, other_482))
        k1401);
  in
    (
      case tmp867_1399 of 
        Nil_517 => (tmp868_1183 here k1398)
        | Cons_518 (tmp872_875, tmp873_874) => (tmp871_1184
          here
          tmp872_875
          tmp873_874
          k1398)
      )
  end;


fun append_487 ev1187_1187 l_485 other_486 k1402 =
  let val tmp878_1403 =
    (reverse_474 ev1187_1187 l_485 (fn a1404 => a1404));
  in (reverseOnto_483 ev1187_1187 tmp878_1403 other_486 k1402)
  end;


fun take_491 ev1188_1188 l_489 n_490 k1405 =
  (if (infixEq_86 n_490 0) then (k1405 Nil_517) else let
    val tmp881_1406 = l_489;
    fun tmp882_1189 ev1191_1191 k1407 = (k1407 Nil_517);
    fun tmp884_1190 ev1192_1192 a_546 rest_547 k1408 =
      let val tmp883_1409 =
        (take_491
          (nested ev1192_1192 ev1188_1188)
          rest_547
          (infixSub_40 n_490 1)
          (fn a1410 =>
            a1410));
      in (k1408 (Cons_518 (a_546, tmp883_1409)))
      end;
  in
    (
      case tmp881_1406 of 
        Nil_517 => (tmp882_1189 here k1405)
        | Cons_518 (tmp885_888, tmp886_887) => (tmp884_1190
          here
          tmp885_888
          tmp886_887
          k1405)
      )
  end);


fun drop_495 ev1193_1193 l_493 n_494 k1411 =
  (if (infixEq_86 n_494 0) then (k1411 l_493) else let
    val tmp893_1412 = l_493;
    fun tmp894_1194 ev1196_1196 k1413 = (k1413 Nil_517);
    fun tmp897_1195 ev1197_1197 a_548 rest_549 k1414 =
      (drop_495
        (nested ev1197_1197 ev1193_1193)
        rest_549
        (infixSub_40 n_494 1)
        k1414);
  in
    (
      case tmp893_1412 of 
        Nil_517 => (tmp894_1194 here k1411)
        | Cons_518 (tmp898_901, tmp899_900) => (tmp897_1195
          here
          tmp898_901
          tmp899_900
          k1411)
      )
  end);


fun nonEmpty_498 ev1198_1198 l_497 k1415 =
  let
    val tmp906_1416 = l_497;
    fun tmp907_1199 ev1201_1201 k1417 = (k1417 false);
    fun tmp908_1200 ev1202_1202 a_550 rest_551 k1418 = (k1418 true);
  in
    (
      case tmp906_1416 of 
        Nil_517 => (tmp907_1199 here k1415)
        | Cons_518 (tmp909_912, tmp910_911) => (tmp908_1200
          here
          tmp909_912
          tmp910_911
          k1415)
      )
  end;


fun head_501 ev1203_1203 ev1204_1204 l_500 ExceptionDollarcapability_1205 k1419 =
  let
    val tmp915_1420 = l_500;
    fun tmp918_1206 ev1208_1208 k1421 =
      ((raise_244 ExceptionDollarcapability_1205)
        (nested ev1208_1208 ev1204_1204)
        EmptyList_516
        "Trying to get the head of an empty list"
        k1421);
    fun tmp919_1207 ev1209_1209 a_552 rest_553 k1422 = (k1422 a_552);
  in
    (
      case tmp915_1420 of 
        Nil_517 => (tmp918_1206 here k1419)
        | Cons_518 (tmp920_923, tmp921_922) => (tmp919_1207
          here
          tmp920_923
          tmp921_922
          k1419)
      )
  end;


fun tail_504 ev1210_1210 ev1211_1211 l_503 ExceptionDollarcapability_1212 k1423 =
  let
    val tmp926_1424 = l_503;
    fun tmp929_1213 ev1215_1215 k1425 =
      ((raise_244 ExceptionDollarcapability_1212)
        (nested ev1215_1215 ev1211_1211)
        EmptyList_516
        "Trying to get the head of an empty list"
        k1425);
    fun tmp930_1214 ev1216_1216 a_554 rest_555 k1426 = (k1426 rest_555);
  in
    (
      case tmp926_1424 of 
        Nil_517 => (tmp929_1213 here k1423)
        | Cons_518 (tmp931_934, tmp932_933) => (tmp930_1214
          here
          tmp931_934
          tmp932_933
          k1423)
      )
  end;


fun headOption_507 ev1217_1217 l_506 k1427 =
  let
    val tmp937_1428 = l_506;
    fun tmp938_1218 ev1220_1220 k1429 = (k1429 None_378);
    fun tmp939_1219 ev1221_1221 a_556 rest_557 k1430 =
      (k1430 (Some_379 a_556));
  in
    (
      case tmp937_1428 of 
        Nil_517 => (tmp938_1218 here k1427)
        | Cons_518 (tmp940_943, tmp941_942) => (tmp939_1219
          here
          tmp940_943
          tmp941_942
          k1427)
      )
  end;


fun partition_511 ev1222_1222 ev1223_1223 l_509 pred_510 k1431 =
  ((withRegion
      (fn ev1224_1224 => fn this_963 => fn k1432 =>
        let
          val tmp946_1433 = Nil_517;
          val lefts_558 = (fresh this_963 tmp946_1433);
          val tmp947_1435 = Nil_517;
          val rights_559 = (fresh this_963 tmp947_1435);
        in
          (foreach_468
            (nested ev1224_1224 ev1222_1222)
            here
            l_509
            (fn ev1225_1225 => fn el_560 => fn k1436 =>
              (pred_510
                (nested (nested ev1225_1225 ev1224_1224) ev1223_1223)
                el_560
                (fn a1437 =>
                  let val tmp948_1438 = a1437;
                  in (if tmp948_1438 then let val tmp949_1439 =
                    (!lefts_558);
                  in (k1436 (lefts_558 := (Cons_518 (el_560, tmp949_1439))))
                  end else let val tmp952_1440 = (!rights_559);
                  in (k1436
                    (rights_559 := (Cons_518 (el_560, tmp952_1440))))
                  end)
                  end)))
            (fn a1441 =>
              let
                val f__1442 = a1441;
                val tmp959_1443 = (!lefts_558);
                val tmp960_1444 =
                  (reverse_474
                    (nested ev1224_1224 ev1222_1222)
                    tmp959_1443
                    (fn a1445 =>
                      a1445));
                val tmp961_1446 = (!rights_559);
                val tmp962_1447 =
                  (reverse_474
                    (nested ev1224_1224 ev1222_1222)
                    tmp961_1446
                    (fn a1448 =>
                      a1448));
              in (k1432 (Tuple2_197 (tmp960_1444, tmp962_1447)))
              end))
        end))
    k1431);


fun show_515 ev1226_1226 ev1227_1227 l_513 showA_514 k1449 =
  let
    val tmp966_1450 = l_513;
    fun tmp967_1228 ev1230_1230 k1451 = (k1451 "Nil()");
    fun tmp971_1229 ev1231_1231 x_561 xs_562 k1452 =
      (showA_514
        (nested ev1231_1231 ev1227_1227)
        x_561
        (fn a1453 =>
          let val tmp968_1454 = a1453;
          in (show_515
            (nested ev1231_1231 ev1226_1226)
            (nested ev1231_1231 ev1227_1227)
            xs_562
            showA_514
            (fn a1455 =>
              let val tmp970_1456 = a1455;
              in (k1452
                (infixConcat_28
                  (infixConcat_28
                    (infixConcat_28
                      (infixConcat_28 "Cons(" tmp968_1454)
                      ", ")
                    tmp970_1456)
                  ")"))
              end))
          end));
  in
    (
      case tmp966_1450 of 
        Nil_517 => (tmp967_1228 here k1449)
        | Cons_518 (tmp972_975, tmp973_974) => (tmp971_1229
          here
          tmp972_975
          tmp973_974
          k1449)
      )
  end;


fun charAt_701 ev1232_1232 str_700 index_699 k1457 =
  (if (infixOr_131
    (infixLt_93 index_699 0)
    (infixLte_96 (length_703 str_700) index_699)) then (k1457
    (Some_379 (unsafeCharAt_722 str_700 index_699))) else (k1457 None_378));


fun substring_712 ev1233_1233 str_710 from_711 k1458 =
  (if (infixOr_131
    (infixLt_93 from_711 0)
    (infixLte_96 (length_703 str_710) from_711)) then (k1458 str_710) else (k1458
    (unsafeSubstring_715 str_710 from_711)));


fun toInt_719 ev1234_1234 str_718 k1459 =
  (toOption_443 ev1234_1234 (internalStringToInt_717 str_718) k1459);


fun call_f_741 ev1235_1235 ev1236_1236 a_739 f_740 k1460 =
  ((((fn ev1238_1238 => fn LogInfoDollarcapability_1239 => fn k1466 =>
          ((LogInfo_744 LogInfoDollarcapability_1239)
            here
            "# before call f()"
            (fn a1467 =>
              let val f__1468 = a1467;
              in (f_740
                (nested ev1238_1238 ev1236_1236)
                a_739
                (fn a1469 =>
                  let val b_747 = a1469;
                  in ((LogInfo_744 LogInfoDollarcapability_1239)
                    here
                    "# after call f()"
                    (fn a1470 =>
                      let val f__1471 = a1470;
                      in (k1466 b_747)
                      end))
                  end))
              end)))
        lift
        (LogInfo_737 (fn ev1237_1237 => fn msg_745 => fn k1461 =>
          ((ev1237_1237
              (fn k1462 =>
                let fun resume_746 ev1464 a1463 = (ev1464 (k1462 a1463));
                in (fn k1465 =>
                  let val _ = (printlnDollar3_11 msg_745);
                  in (resume_746 here () k1465)
                  end)
                end))
            k1461))))
      (fn a1472 =>
        (fn k21473 =>
          (k21473 a1472))))
    k1460);


fun main_742 ev1240_1240 k1474 =
  ((((fn ev1242_1242 => fn LogInfoDollarcapability_1243 => fn k1480 =>
          (call_f_741
            (nested ev1242_1242 ev1240_1240)
            here
            1
            (fn ev1244_1244 => fn f__750 => fn k1481 =>
              ((LogInfo_744 LogInfoDollarcapability_1243)
                ev1244_1244
                "# do not print to console"
                k1481))
            k1480))
        lift
        (LogInfo_737 (fn ev1241_1241 => fn f__748 => fn k1475 =>
          ((ev1241_1241
              (fn k1476 =>
                let fun resume_749 ev1478 a1477 = (ev1478 (k1476 a1477));
                in (fn k1479 => (resume_749 here () k1479))
                end))
            k1475))))
      (fn a1482 =>
        (fn k21483 =>
          (k21483 a1482))))
    k1474);

(main_742 (fn a => a) (fn a => a));
