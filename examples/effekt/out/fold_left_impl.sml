datatype ('arg1354, 'arg1355) Tuple2_137 =
  Tuple2_197 of ('arg1354 * 'arg1355);


fun first_200 (Tuple2_197 (first_200, _)) = first_200;


fun second_201 (Tuple2_197 (_, second_201)) = second_201;


datatype ('arg1356, 'arg1357, 'arg1358) Tuple3_141 =
  Tuple3_202 of ('arg1356 * 'arg1357 * 'arg1358);


fun firstDollar1_206 (Tuple3_202 (firstDollar1_206, _, _)) =
  firstDollar1_206;


fun secondDollar1_207 (Tuple3_202 (_, secondDollar1_207, _)) =
  secondDollar1_207;


fun third_208 (Tuple3_202 (_, _, third_208)) = third_208;


datatype ('arg1359, 'arg1360, 'arg1361, 'arg1362) Tuple4_146 =
  Tuple4_209 of ('arg1359 * 'arg1360 * 'arg1361 * 'arg1362);


fun firstDollar2_214 (Tuple4_209 (firstDollar2_214, _, _, _)) =
  firstDollar2_214;


fun secondDollar2_215 (Tuple4_209 (_, secondDollar2_215, _, _)) =
  secondDollar2_215;


fun thirdDollar1_216 (Tuple4_209 (_, _, thirdDollar1_216, _)) =
  thirdDollar1_216;


fun fourth_217 (Tuple4_209 (_, _, _, fourth_217)) = fourth_217;


datatype ('arg1363, 'arg1364, 'arg1365, 'arg1366, 'arg1367) Tuple5_152 =
  Tuple5_218 of ('arg1363 * 'arg1364 * 'arg1365 * 'arg1366 * 'arg1367);


fun firstDollar3_224 (Tuple5_218 (firstDollar3_224, _, _, _, _)) =
  firstDollar3_224;


fun secondDollar3_225 (Tuple5_218 (_, secondDollar3_225, _, _, _)) =
  secondDollar3_225;


fun thirdDollar2_226 (Tuple5_218 (_, _, thirdDollar2_226, _, _)) =
  thirdDollar2_226;


fun fourthDollar1_227 (Tuple5_218 (_, _, _, fourthDollar1_227, _)) =
  fourthDollar1_227;


fun fifth_228 (Tuple5_218 (_, _, _, _, fifth_228)) = fifth_228;


datatype ('arg1368, 'arg1369, 'arg1370, 'arg1371, 'arg1372, 'arg1373) Tuple6_159 =
  Tuple6_229 of ('arg1368 * 'arg1369 * 'arg1370 * 'arg1371 * 'arg1372 * 'arg1373);


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


datatype 'arg1374 Exception_164 =
  Exception_164 of 'arg1374;


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


datatype 'arg1375 NoSuchElementException_807 =
  NoSuchElementException_807 of 'arg1375;


fun NoSuchElementException_808 (NoSuchElementException_807 NoSuchElementException_808) =
  NoSuchElementException_808;


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


fun emptyArray_739 () = (Array.fromList []);


fun array_743 initialSize init = (Array.array (initialSize, init));


fun size_750 arr = (Array.length arr);


fun unsafeGet_754 arr index = (Array.sub (arr, index));


fun unsafeput_764 arr index value = (Array.update (arr, index, value));


fun copyInto_771 src dst = (Array.copy{src = src, dst = dst, di = 0});


fun locally_3 ev1177_1177 ev1178_1178 f_1 k1376 = (f_1 ev1178_1178 k1376);


val PI_196 = (f_pi_74 ());


fun raise_168 ev1179_1179 ev1180_1180 msg_167 ExceptionDollarcapability_1181 k1377 =
  ((raise_244 ExceptionDollarcapability_1181)
    ev1180_1180
    RuntimeError_245
    msg_167
    (fn a1378 =>
      let
        val tmp1127_1379 = a1378;
        val tmp1128_1380 = tmp1127_1379;
      in (raise Hole)
      end));


fun raiseDollar1_173 ev1183_1183 ev1184_1184 exception_171 msg_172 ExceptionDollarcapability_1185 k1381 =
  ((raise_244 ExceptionDollarcapability_1185)
    ev1184_1184
    exception_171
    msg_172
    (fn a1382 =>
      let
        val tmp1131_1383 = a1382;
        val tmp1132_1384 = tmp1131_1383;
      in (raise Hole)
      end));


fun panicOn_176 ev1186_1186 ev1187_1187 prog_175 k1385 =
  ((((fn ev1189_1189 => fn ExceptionDollarcapability_1190 => fn k1392 =>
          (prog_175
            (nested ev1189_1189 ev1187_1187)
            here
            ExceptionDollarcapability_1190
            k1392))
        lift
        (Exception_164 (fn ev1188_1188 => fn exception_246 => fn msg_247 => fn k1386 =>
          ((ev1188_1188
              (fn k1387 =>
                let fun resume_248 ev1389 a1388 = (ev1389 (k1387 a1388));
                in (fn k1390 =>
                  let val tmp1137_1391 = (panic_162 msg_247);
                  in (k1390 tmp1137_1391)
                  end)
                end))
            k1386))))
      (fn a1393 =>
        (fn k21394 =>
          (k21394 a1393))))
    k1385);


fun report_179 ev1191_1191 ev1192_1192 prog_178 k1395 =
  ((((fn ev1194_1194 => fn ExceptionDollarcapability_1195 => fn k1402 =>
          (prog_178
            (nested ev1194_1194 ev1192_1192)
            here
            ExceptionDollarcapability_1195
            k1402))
        lift
        (Exception_164 (fn ev1193_1193 => fn exception_249 => fn msg_250 => fn k1396 =>
          ((ev1193_1193
              (fn k1397 =>
                let fun resume_251 ev1399 a1398 = (ev1399 (k1397 a1398));
                in (fn k1400 =>
                  let val tmp1142_1401 = (printlnDollar3_11 msg_250);
                  in (k1400 tmp1142_1401)
                  end)
                end))
            k1396))))
      (fn a1403 =>
        (fn k21404 =>
          (k21404 a1403))))
    k1395);


fun ignoring_182 ev1196_1196 ev1197_1197 prog_181 k1405 =
  ((((fn ev1199_1199 => fn ExceptionDollarcapability_1200 => fn k1411 =>
          (prog_181
            (nested ev1199_1199 ev1197_1197)
            here
            ExceptionDollarcapability_1200
            k1411))
        lift
        (Exception_164 (fn ev1198_1198 => fn exception_252 => fn msg_253 => fn k1406 =>
          ((ev1198_1198
              (fn k1407 =>
                let fun resume_254 ev1409 a1408 = (ev1409 (k1407 a1408));
                in (fn k1410 => (k1410 ()))
                end))
            k1406))))
      (fn a1412 =>
        (fn k21413 =>
          (k21413 a1412))))
    k1405);


fun each_186 ev1201_1201 ev1202_1202 start_183 end_184 action_185 k1414 =
  let fun loop_256 ev1203_1203 i_255 k1415 =
    (if (infixLt_93 i_255 end_184) then (action_185
      (nested ev1203_1203 ev1202_1202)
      i_255
      (fn a1416 =>
        let val f__1417 = a1416;
        in (loop_256 ev1203_1203 (infixAdd_31 i_255 1) k1415)
        end)) else (k1415 ()));
  in (loop_256 here start_183 k1414)
  end;


fun repeat_189 ev1204_1204 ev1205_1205 n_187 action_188 k1418 =
  (each_186
    ev1204_1204
    here
    0
    n_187
    (fn ev1206_1206 => fn n_257 => fn k1419 =>
      (action_188 (nested ev1206_1206 ev1205_1205) k1419))
    k1418);


fun timed_191 ev1207_1207 ev1208_1208 block_190 k1420 =
  let val before_258 =
    let val tmp1161_1421 = (timestamp_15 ());
    in tmp1161_1421
    end;
  in (block_190
    ev1208_1208
    (fn a1422 =>
      let
        val f__1423 = a1422;
        val after_259 =
          let val tmp1164_1424 = (timestamp_15 ());
          in tmp1164_1424
          end;
      in (k1420 (infixMul_34 1000 (infixSub_40 after_259 before_258)))
      end))
  end;


fun measure_195 ev1209_1209 ev1210_1210 warmup_192 iterations_193 block_194 k1425 =
  let
    fun run_262 ev1211_1211 n_260 report_261 k1426 =
      (if (infixLte_96 n_260 0) then (k1426 ()) else (timed_191
        (nested ev1211_1211 ev1209_1209)
        here
        (fn ev1212_1212 => fn k1427 =>
          (block_194
            (nested (nested ev1212_1212 ev1211_1211) ev1210_1210)
            k1427))
        (fn a1428 =>
          let
            val time_263 = a1428;
            val k1429 =
              (fn a1430 =>
                let
                  val tmp1170_1431 = a1430;
                  val _ = tmp1170_1431;
                in
                  (run_262
                    ev1211_1211
                    (infixSub_40 n_260 1)
                    report_261
                    k1426)
                end);
          in
            (if report_261 then let val tmp1169_1432 =
              (println_5 time_263);
            in (k1429 tmp1169_1432)
            end else (k1429 ()))
          end)));
    val _ = (run_262 here warmup_192 false (fn a1433 => a1433));
  in (run_262 here iterations_193 true k1425)
  end;


fun isDefined_353 ev1213_1213 self_352 k1434 =
  let
    val tmp1080_1435 = self_352;
    fun tmp1081_1214 ev1216_1216 k1436 = (k1436 false);
    fun tmp1082_1215 ev1217_1217 v_382 k1437 = (k1437 true);
  in
    (
      case tmp1080_1435 of 
        None_378 => (tmp1081_1214 here k1434)
        | Some_379 tmp1083_1438 => (tmp1082_1215 here tmp1083_1438 k1434)
      )
  end;


fun isEmpty_356 ev1218_1218 self_355 k1439 =
  let val tmp1086_1440 =
    (isDefined_353 ev1218_1218 self_355 (fn a1441 => a1441));
  in (k1439 (not_128 tmp1086_1440))
  end;


fun orElse_360 ev1219_1219 ev1220_1220 self_358 that_359 k1442 =
  let
    val tmp1087_1443 = self_358;
    fun tmp1090_1221 ev1223_1223 k1444 =
      (that_359 (nested ev1223_1223 ev1220_1220) k1444);
    fun tmp1091_1222 ev1224_1224 v_383 k1445 = (k1445 (Some_379 v_383));
  in
    (
      case tmp1087_1443 of 
        None_378 => (tmp1090_1221 here k1442)
        | Some_379 tmp1092_1446 => (tmp1091_1222 here tmp1092_1446 k1442)
      )
  end;


fun getOrElse_364 ev1225_1225 ev1226_1226 self_362 that_363 k1447 =
  let
    val tmp1095_1448 = self_362;
    fun tmp1098_1227 ev1229_1229 k1449 =
      (that_363 (nested ev1229_1229 ev1226_1226) k1449);
    fun tmp1099_1228 ev1230_1230 v_384 k1450 = (k1450 v_384);
  in
    (
      case tmp1095_1448 of 
        None_378 => (tmp1098_1227 here k1447)
        | Some_379 tmp1100_1451 => (tmp1099_1228 here tmp1100_1451 k1447)
      )
  end;


fun map_369 ev1231_1231 ev1232_1232 self_367 f_368 k1452 =
  let
    val tmp1103_1453 = self_367;
    fun tmp1104_1233 ev1235_1235 k1454 = (k1454 None_378);
    fun tmp1106_1234 ev1236_1236 v_385 k1455 =
      (f_368
        (nested ev1236_1236 ev1232_1232)
        v_385
        (fn a1456 =>
          let val tmp1105_1457 = a1456;
          in (k1455 (Some_379 tmp1105_1457))
          end));
  in
    (
      case tmp1103_1453 of 
        None_378 => (tmp1104_1233 here k1452)
        | Some_379 tmp1107_1458 => (tmp1106_1234 here tmp1107_1458 k1452)
      )
  end;


fun foreach_373 ev1237_1237 ev1238_1238 self_371 f_372 k1459 =
  let
    val tmp1110_1460 = self_371;
    fun tmp1111_1239 ev1241_1241 k1461 = (k1461 ());
    fun tmp1114_1240 ev1242_1242 v_386 k1462 =
      (f_372 (nested ev1242_1242 ev1238_1238) v_386 k1462);
  in
    (
      case tmp1110_1460 of 
        None_378 => (tmp1111_1239 here k1459)
        | Some_379 tmp1115_1463 => (tmp1114_1240 here tmp1115_1463 k1459)
      )
  end;


fun show_377 ev1243_1243 ev1244_1244 o_375 showA_376 k1464 =
  let
    val tmp1118_1465 = o_375;
    fun tmp1119_1245 ev1247_1247 k1466 = (k1466 "None()");
    fun tmp1121_1246 ev1248_1248 v_387 k1467 =
      (showA_376
        (nested ev1248_1248 ev1244_1244)
        v_387
        (fn a1468 =>
          let val tmp1120_1469 = a1468;
          in (k1467
            (infixConcat_28 (infixConcat_28 "Some(" tmp1120_1469) ")"))
          end));
  in
    (
      case tmp1118_1465 of 
        None_378 => (tmp1119_1245 here k1464)
        | Some_379 tmp1122_1470 => (tmp1121_1246 here tmp1122_1470 k1464)
      )
  end;


fun isEmpty_437 ev1249_1249 opt_436 k1471 =
  (k1471 (not_128 (isDefined_434 opt_436)));


fun toOption_443 ev1250_1250 opt_442 k1472 =
  (if (isDefined_434 opt_442) then (k1472 (Some_379 (force_440 opt_442))) else (k1472
    None_378));


fun isEmpty_464 ev1251_1251 l_463 k1473 =
  let
    val tmp899_1474 = l_463;
    fun tmp900_1252 ev1254_1254 k1475 = (k1475 true);
    fun tmp901_1253 ev1255_1255 a_523 rest_524 k1476 = (k1476 false);
  in
    (
      case tmp899_1474 of 
        Nil_517 => (tmp900_1252 here k1473)
        | Cons_518 (tmp902_905, tmp903_904) => (tmp901_1253
          here
          tmp902_905
          tmp903_904
          k1473)
      )
  end;


fun foreach_468 ev1256_1256 ev1257_1257 l_466 f_467 k1477 =
  let fun loop_526 ev1258_1258 remainder_525 k1478 =
    let val tmp908_1479 =
      (isEmpty_464
        (nested ev1258_1258 ev1256_1256)
        remainder_525
        (fn a1480 =>
          a1480));
    in (if (not_128 tmp908_1479) then let
      val tmp909_1481 = remainder_525;
      fun tmp910_1259 ev1261_1261 k1482 = (k1482 ());
      fun tmp915_1260 ev1262_1262 a_527 as_528 k1483 =
        (f_467
          (nested (nested ev1262_1262 ev1258_1258) ev1257_1257)
          a_527
          (fn a1484 =>
            let val f__1485 = a1484;
            in (loop_526 (nested ev1262_1262 ev1258_1258) as_528 k1483)
            end));
    in
      (
        case tmp909_1481 of 
          Nil_517 => (tmp910_1259 here k1478)
          | Cons_518 (tmp916_919, tmp917_918) => (tmp915_1260
            here
            tmp916_919
            tmp917_918
            k1478)
        )
    end else (k1478 ()))
    end;
  in (loop_526 here l_466 k1477)
  end;


fun size_471 ev1263_1263 l_470 k1486 =
  let fun loop_531 ev1264_1264 lst_529 acc_530 k1487 =
    let
      val tmp926_1488 = lst_529;
      fun tmp927_1265 ev1267_1267 k1489 = (k1489 acc_530);
      fun tmp930_1266 ev1268_1268 a_532 as_533 k1490 =
        (loop_531
          (nested ev1268_1268 ev1264_1264)
          as_533
          (infixAdd_31 acc_530 1)
          k1490);
    in
      (
        case tmp926_1488 of 
          Nil_517 => (tmp927_1265 here k1487)
          | Cons_518 (tmp931_934, tmp932_933) => (tmp930_1266
            here
            tmp931_934
            tmp932_933
            k1487)
        )
    end;
  in (loop_531 here l_470 0 k1486)
  end;


fun reverse_474 ev1269_1269 l_473 k1491 =
  let fun loop_536 ev1270_1270 lst_534 acc_535 k1492 =
    let
      val tmp939_1493 = lst_534;
      fun tmp940_1271 ev1273_1273 k1494 = (k1494 acc_535);
      fun tmp943_1272 ev1274_1274 a_537 as_538 k1495 =
        (loop_536
          (nested ev1274_1274 ev1270_1270)
          as_538
          (Cons_518 (a_537, acc_535))
          k1495);
    in
      (
        case tmp939_1493 of 
          Nil_517 => (tmp940_1271 here k1492)
          | Cons_518 (tmp944_947, tmp945_946) => (tmp943_1272
            here
            tmp944_947
            tmp945_946
            k1492)
        )
    end;
  in (loop_536 here l_473 Nil_517 k1491)
  end;


fun map_479 ev1275_1275 ev1276_1276 l_477 f_478 k1496 =
  let fun loop_541 ev1277_1277 lst_539 acc_540 k1497 =
    let
      val tmp952_1498 = lst_539;
      fun tmp953_1278 ev1280_1280 k1499 = (k1499 acc_540);
      fun tmp957_1279 ev1281_1281 a_542 as_543 k1500 =
        (f_478
          (nested (nested ev1281_1281 ev1277_1277) ev1276_1276)
          a_542
          (fn a1501 =>
            let val tmp954_1502 = a1501;
            in (loop_541
              (nested ev1281_1281 ev1277_1277)
              as_543
              (Cons_518 (tmp954_1502, acc_540))
              k1500)
            end));
    in
      (
        case tmp952_1498 of 
          Nil_517 => (tmp953_1278 here k1497)
          | Cons_518 (tmp958_961, tmp959_960) => (tmp957_1279
            here
            tmp958_961
            tmp959_960
            k1497)
        )
    end;
  in (loop_541
    here
    l_477
    Nil_517
    (fn a1503 =>
      let val tmp964_1504 = a1503;
      in (reverse_474 ev1275_1275 tmp964_1504 k1496)
      end))
  end;


fun reverseOnto_483 ev1282_1282 l_481 other_482 k1505 =
  let
    val tmp967_1506 = l_481;
    fun tmp968_1283 ev1285_1285 k1507 = (k1507 other_482);
    fun tmp971_1284 ev1286_1286 a_544 rest_545 k1508 =
      (reverseOnto_483
        (nested ev1286_1286 ev1282_1282)
        rest_545
        (Cons_518 (a_544, other_482))
        k1508);
  in
    (
      case tmp967_1506 of 
        Nil_517 => (tmp968_1283 here k1505)
        | Cons_518 (tmp972_975, tmp973_974) => (tmp971_1284
          here
          tmp972_975
          tmp973_974
          k1505)
      )
  end;


fun append_487 ev1287_1287 l_485 other_486 k1509 =
  let val tmp978_1510 =
    (reverse_474 ev1287_1287 l_485 (fn a1511 => a1511));
  in (reverseOnto_483 ev1287_1287 tmp978_1510 other_486 k1509)
  end;


fun take_491 ev1288_1288 l_489 n_490 k1512 =
  (if (infixEq_86 n_490 0) then (k1512 Nil_517) else let
    val tmp981_1513 = l_489;
    fun tmp982_1289 ev1291_1291 k1514 = (k1514 Nil_517);
    fun tmp984_1290 ev1292_1292 a_546 rest_547 k1515 =
      let val tmp983_1516 =
        (take_491
          (nested ev1292_1292 ev1288_1288)
          rest_547
          (infixSub_40 n_490 1)
          (fn a1517 =>
            a1517));
      in (k1515 (Cons_518 (a_546, tmp983_1516)))
      end;
  in
    (
      case tmp981_1513 of 
        Nil_517 => (tmp982_1289 here k1512)
        | Cons_518 (tmp985_988, tmp986_987) => (tmp984_1290
          here
          tmp985_988
          tmp986_987
          k1512)
      )
  end);


fun drop_495 ev1293_1293 l_493 n_494 k1518 =
  (if (infixEq_86 n_494 0) then (k1518 l_493) else let
    val tmp993_1519 = l_493;
    fun tmp994_1294 ev1296_1296 k1520 = (k1520 Nil_517);
    fun tmp997_1295 ev1297_1297 a_548 rest_549 k1521 =
      (drop_495
        (nested ev1297_1297 ev1293_1293)
        rest_549
        (infixSub_40 n_494 1)
        k1521);
  in
    (
      case tmp993_1519 of 
        Nil_517 => (tmp994_1294 here k1518)
        | Cons_518 (tmp998_1001, tmp999_1000) => (tmp997_1295
          here
          tmp998_1001
          tmp999_1000
          k1518)
      )
  end);


fun nonEmpty_498 ev1298_1298 l_497 k1522 =
  let
    val tmp1006_1523 = l_497;
    fun tmp1007_1299 ev1301_1301 k1524 = (k1524 false);
    fun tmp1008_1300 ev1302_1302 a_550 rest_551 k1525 = (k1525 true);
  in
    (
      case tmp1006_1523 of 
        Nil_517 => (tmp1007_1299 here k1522)
        | Cons_518 (tmp1009_1012, tmp1010_1011) => (tmp1008_1300
          here
          tmp1009_1012
          tmp1010_1011
          k1522)
      )
  end;


fun head_501 ev1303_1303 ev1304_1304 l_500 ExceptionDollarcapability_1305 k1526 =
  let
    val tmp1015_1527 = l_500;
    fun tmp1018_1306 ev1308_1308 k1528 =
      ((raise_244 ExceptionDollarcapability_1305)
        (nested ev1308_1308 ev1304_1304)
        EmptyList_516
        "Trying to get the head of an empty list"
        k1528);
    fun tmp1019_1307 ev1309_1309 a_552 rest_553 k1529 = (k1529 a_552);
  in
    (
      case tmp1015_1527 of 
        Nil_517 => (tmp1018_1306 here k1526)
        | Cons_518 (tmp1020_1023, tmp1021_1022) => (tmp1019_1307
          here
          tmp1020_1023
          tmp1021_1022
          k1526)
      )
  end;


fun tail_504 ev1310_1310 ev1311_1311 l_503 ExceptionDollarcapability_1312 k1530 =
  let
    val tmp1026_1531 = l_503;
    fun tmp1029_1313 ev1315_1315 k1532 =
      ((raise_244 ExceptionDollarcapability_1312)
        (nested ev1315_1315 ev1311_1311)
        EmptyList_516
        "Trying to get the head of an empty list"
        k1532);
    fun tmp1030_1314 ev1316_1316 a_554 rest_555 k1533 = (k1533 rest_555);
  in
    (
      case tmp1026_1531 of 
        Nil_517 => (tmp1029_1313 here k1530)
        | Cons_518 (tmp1031_1034, tmp1032_1033) => (tmp1030_1314
          here
          tmp1031_1034
          tmp1032_1033
          k1530)
      )
  end;


fun headOption_507 ev1317_1317 l_506 k1534 =
  let
    val tmp1037_1535 = l_506;
    fun tmp1038_1318 ev1320_1320 k1536 = (k1536 None_378);
    fun tmp1039_1319 ev1321_1321 a_556 rest_557 k1537 =
      (k1537 (Some_379 a_556));
  in
    (
      case tmp1037_1535 of 
        Nil_517 => (tmp1038_1318 here k1534)
        | Cons_518 (tmp1040_1043, tmp1041_1042) => (tmp1039_1319
          here
          tmp1040_1043
          tmp1041_1042
          k1534)
      )
  end;


fun partition_511 ev1322_1322 ev1323_1323 l_509 pred_510 k1538 =
  ((withRegion
      (fn ev1324_1324 => fn this_1063 => fn k1539 =>
        let
          val tmp1046_1540 = Nil_517;
          val lefts_558 = (fresh this_1063 tmp1046_1540);
          val tmp1047_1542 = Nil_517;
          val rights_559 = (fresh this_1063 tmp1047_1542);
        in
          (foreach_468
            (nested ev1324_1324 ev1322_1322)
            here
            l_509
            (fn ev1325_1325 => fn el_560 => fn k1543 =>
              (pred_510
                (nested (nested ev1325_1325 ev1324_1324) ev1323_1323)
                el_560
                (fn a1544 =>
                  let val tmp1048_1545 = a1544;
                  in (if tmp1048_1545 then let val tmp1049_1546 =
                    (!lefts_558);
                  in (k1543
                    (lefts_558 := (Cons_518 (el_560, tmp1049_1546))))
                  end else let val tmp1052_1547 = (!rights_559);
                  in (k1543
                    (rights_559 := (Cons_518 (el_560, tmp1052_1547))))
                  end)
                  end)))
            (fn a1548 =>
              let
                val f__1549 = a1548;
                val tmp1059_1550 = (!lefts_558);
                val tmp1060_1551 =
                  (reverse_474
                    (nested ev1324_1324 ev1322_1322)
                    tmp1059_1550
                    (fn a1552 =>
                      a1552));
                val tmp1061_1553 = (!rights_559);
                val tmp1062_1554 =
                  (reverse_474
                    (nested ev1324_1324 ev1322_1322)
                    tmp1061_1553
                    (fn a1555 =>
                      a1555));
              in (k1539 (Tuple2_197 (tmp1060_1551, tmp1062_1554)))
              end))
        end))
    k1538);


fun show_515 ev1326_1326 ev1327_1327 l_513 showA_514 k1556 =
  let
    val tmp1066_1557 = l_513;
    fun tmp1067_1328 ev1330_1330 k1558 = (k1558 "Nil()");
    fun tmp1071_1329 ev1331_1331 x_561 xs_562 k1559 =
      (showA_514
        (nested ev1331_1331 ev1327_1327)
        x_561
        (fn a1560 =>
          let val tmp1068_1561 = a1560;
          in (show_515
            (nested ev1331_1331 ev1326_1326)
            (nested ev1331_1331 ev1327_1327)
            xs_562
            showA_514
            (fn a1562 =>
              let val tmp1070_1563 = a1562;
              in (k1559
                (infixConcat_28
                  (infixConcat_28
                    (infixConcat_28
                      (infixConcat_28 "Cons(" tmp1068_1561)
                      ", ")
                    tmp1070_1563)
                  ")"))
              end))
          end));
  in
    (
      case tmp1066_1557 of 
        Nil_517 => (tmp1067_1328 here k1556)
        | Cons_518 (tmp1072_1075, tmp1073_1074) => (tmp1071_1329
          here
          tmp1072_1075
          tmp1073_1074
          k1556)
      )
  end;


fun charAt_701 ev1332_1332 str_700 index_699 k1564 =
  (if (infixOr_131
    (infixLt_93 index_699 0)
    (infixLte_96 (length_703 str_700) index_699)) then (k1564
    (Some_379 (unsafeCharAt_722 str_700 index_699))) else (k1564 None_378));


fun substring_712 ev1333_1333 str_710 from_711 k1565 =
  (if (infixOr_131
    (infixLt_93 from_711 0)
    (infixLte_96 (length_703 str_710) from_711)) then (k1565 str_710) else (k1565
    (unsafeSubstring_715 str_710 from_711)));


fun toInt_719 ev1334_1334 str_718 k1566 =
  (toOption_443 ev1334_1334 (internalStringToInt_717 str_718) k1566);


fun get_747 ev1335_1335 arr_745 index_746 k1567 =
  let val len_772 = (size_750 arr_745);
  in (if (infixOr_131
    (infixLt_93 index_746 0)
    (infixLte_96 len_772 index_746)) then (k1567 None_378) else (k1567
    (Some_379 (unsafeGet_754 arr_745 index_746))))
  end;


fun put_759 ev1336_1336 arr_756 index_757 value_758 k1568 =
  (if (infixOr_131
    (infixLt_93 index_757 0)
    (infixLte_96 (size_750 arr_756) index_757)) then (k1568 ()) else let val tmp883_1569 =
    (unsafeput_764 arr_756 index_757 value_758);
  in (k1568 tmp883_1569)
  end);


fun copy_767 ev1337_1337 arr_766 k1570 =
  let
    val tmp886_1571 = (get_747 ev1337_1337 arr_766 0 (fn a1572 => a1572));
    val tmp887_1573 = tmp886_1571;
    fun tmp888_1338 ev1340_1340 k1574 = (k1574 (emptyArray_739 ()));
    fun tmp889_1339 ev1341_1341 first_773 k1575 =
      let
        val dst_774 = (array_743 (size_750 arr_766) first_773);
        val _ = (copyInto_771 arr_766 dst_774);
      in (k1575 dst_774)
      end;
  in
    (
      case tmp887_1573 of 
        None_378 => (tmp888_1338 here k1570)
        | Some_379 tmp890_1576 => (tmp889_1339 here tmp890_1576 k1570)
      )
  end;


fun main_806 ev1342_1342 k1577 =
  ((withRegion
      (fn ev1343_1343 => fn this_878 => fn k1578 =>
        let
          val arrLen_809 = 100100100;
          val arr_810 = (array_743 arrLen_809 0);
          val tmp846_1579 = 0;
          val iterIdx_811 = (fresh this_878 tmp846_1579);
          fun iterNext_812 ev1345_1345 ev1346_1346 NoSuchElementExceptionDollarcapability_1347 k1580 =
            let val tmp847_1581 = (!iterIdx_811);
            in (if (infixLt_93 tmp847_1581 arrLen_809) then let
              val tmp849_1582 = (!iterIdx_811);
              val tmp850_1583 =
                (iterIdx_811 := (infixAdd_31 tmp849_1582 1));
              val _ = tmp850_1583;
              val tmp851_1584 = (!iterIdx_811);
            in (k1580 (unsafeGet_754 arr_810 (infixSub_40 tmp851_1584 1)))
            end else ((NoSuchElementException_808
                NoSuchElementExceptionDollarcapability_1347)
              ev1346_1346
              k1580))
            end;
          fun foldLeft_815 ev1348_1348 ev1349_1349 acc_813 f_814 k1585 =
            ((withRegion
                (fn ev1350_1350 => fn this_873 => fn k1586 =>
                  let
                    val tmp856_1587 = false;
                    val break_816 = (fresh this_873 tmp856_1587);
                    val tmp857_1588 = 0;
                    val next_817 = (fresh this_873 tmp857_1588);
                  in
                    ((((fn ev1352_1352 => fn NoSuchElementExceptionDollarcapability_1353 => fn k1594 =>
                            (iterNext_812
                              (nested
                                (nested ev1352_1352 ev1350_1350)
                                ev1348_1348)
                              here
                              NoSuchElementExceptionDollarcapability_1353
                              (fn a1595 =>
                                let val tmp858_1596 = a1595;
                                in (k1594 (next_817 := tmp858_1596))
                                end)))
                          lift
                          (NoSuchElementException_807 (fn ev1351_1351 => fn k1589 =>
                            ((ev1351_1351
                                (fn k1590 =>
                                  let fun resume_818 ev1592 a1591 =
                                    (ev1592 (k1590 a1591));
                                  in (fn k1593 =>
                                    (k1593 (break_816 := true)))
                                  end))
                              k1589))))
                        (fn a1597 =>
                          (fn k21598 =>
                            (k21598 a1597))))
                      (fn a1599 =>
                        let
                          val tmp863_1600 = a1599;
                          val _ = tmp863_1600;
                          val tmp864_1601 = (!break_816);
                        in
                          (if tmp864_1601 then (k1586 acc_813) else let val tmp865_1602 =
                            (!next_817);
                          in (f_814
                            (nested ev1350_1350 ev1349_1349)
                            acc_813
                            tmp865_1602
                            (fn a1603 =>
                              let val tmp866_1604 = a1603;
                              in (foldLeft_815
                                (nested ev1350_1350 ev1348_1348)
                                (nested ev1350_1350 ev1349_1349)
                                tmp866_1604
                                f_814
                                k1586)
                              end))
                          end)
                        end))
                  end))
              k1585);
        in
          (foldLeft_815
            here
            here
            0
            (fn ev1344_1344 => fn acc_819 => fn next_820 => fn k1605 =>
              (k1605 (infixAdd_31 acc_819 next_820)))
            k1578)
        end))
    k1577);

(main_806 (fn a => a) (fn a => a));
