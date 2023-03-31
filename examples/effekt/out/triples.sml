datatype ('arg1319, 'arg1320) Tuple2_137 =
  Tuple2_197 of ('arg1319 * 'arg1320);


fun first_200 (Tuple2_197 (first_200, _)) = first_200;


fun second_201 (Tuple2_197 (_, second_201)) = second_201;


datatype ('arg1321, 'arg1322, 'arg1323) Tuple3_141 =
  Tuple3_202 of ('arg1321 * 'arg1322 * 'arg1323);


fun firstDollar1_206 (Tuple3_202 (firstDollar1_206, _, _)) =
  firstDollar1_206;


fun secondDollar1_207 (Tuple3_202 (_, secondDollar1_207, _)) =
  secondDollar1_207;


fun third_208 (Tuple3_202 (_, _, third_208)) = third_208;


datatype ('arg1324, 'arg1325, 'arg1326, 'arg1327) Tuple4_146 =
  Tuple4_209 of ('arg1324 * 'arg1325 * 'arg1326 * 'arg1327);


fun firstDollar2_214 (Tuple4_209 (firstDollar2_214, _, _, _)) =
  firstDollar2_214;


fun secondDollar2_215 (Tuple4_209 (_, secondDollar2_215, _, _)) =
  secondDollar2_215;


fun thirdDollar1_216 (Tuple4_209 (_, _, thirdDollar1_216, _)) =
  thirdDollar1_216;


fun fourth_217 (Tuple4_209 (_, _, _, fourth_217)) = fourth_217;


datatype ('arg1328, 'arg1329, 'arg1330, 'arg1331, 'arg1332) Tuple5_152 =
  Tuple5_218 of ('arg1328 * 'arg1329 * 'arg1330 * 'arg1331 * 'arg1332);


fun firstDollar3_224 (Tuple5_218 (firstDollar3_224, _, _, _, _)) =
  firstDollar3_224;


fun secondDollar3_225 (Tuple5_218 (_, secondDollar3_225, _, _, _)) =
  secondDollar3_225;


fun thirdDollar2_226 (Tuple5_218 (_, _, thirdDollar2_226, _, _)) =
  thirdDollar2_226;


fun fourthDollar1_227 (Tuple5_218 (_, _, _, fourthDollar1_227, _)) =
  fourthDollar1_227;


fun fifth_228 (Tuple5_218 (_, _, _, _, fifth_228)) = fifth_228;


datatype ('arg1333, 'arg1334, 'arg1335, 'arg1336, 'arg1337, 'arg1338) Tuple6_159 =
  Tuple6_229 of ('arg1333 * 'arg1334 * 'arg1335 * 'arg1336 * 'arg1337 * 'arg1338);


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


datatype 'arg1339 Exception_164 =
  Exception_164 of 'arg1339;


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


datatype 'arg1340 Choose_737 =
  Choose_737 of 'arg1340;


fun choose_751 (Choose_737 choose_751) = choose_751;


datatype 'arg1341 Fail_736 =
  Fail_736 of 'arg1341;


fun fail_752 (Fail_736 fail_752) = fail_752;


datatype 'arg1342 Yield_739 =
  Yield_739 of 'arg1342;


fun yield_756 (Yield_739 yield_756) = yield_756;


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


fun locally_3 ev1137_1137 ev1138_1138 f_1 k1343 = (f_1 ev1138_1138 k1343);


val PI_196 = (f_pi_74 ());


fun raise_168 ev1139_1139 ev1140_1140 msg_167 ExceptionDollarcapability_1141 k1344 =
  ((raise_244 ExceptionDollarcapability_1141)
    ev1140_1140
    RuntimeError_245
    msg_167
    (fn a1347 =>
      let
        val tmp1087_1348 = a1347;
        val tmp1088_1349 = tmp1087_1348;
      in (raise Hole)
      end));


fun raiseDollar1_173 ev1143_1143 ev1144_1144 exception_171 msg_172 ExceptionDollarcapability_1145 k1350 =
  ((raise_244 ExceptionDollarcapability_1145)
    ev1144_1144
    exception_171
    msg_172
    (fn a1351 =>
      let
        val tmp1091_1352 = a1351;
        val tmp1092_1353 = tmp1091_1352;
      in (raise Hole)
      end));


fun panicOn_176 ev1146_1146 ev1147_1147 prog_175 k1354 =
  ((((fn ev1149_1149 => fn ExceptionDollarcapability_1150 => fn k1361 =>
          (prog_175
            (nested ev1149_1149 ev1147_1147)
            here
            ExceptionDollarcapability_1150
            k1361))
        lift
        (Exception_164 (fn ev1148_1148 => fn exception_246 => fn msg_247 => fn k1355 =>
          ((ev1148_1148
              (fn k1356 =>
                let fun resume_248 ev1358 a1357 = (ev1358 (k1356 a1357));
                in (fn k1359 =>
                  let val tmp1097_1360 = (panic_162 msg_247);
                  in (k1359 tmp1097_1360)
                  end)
                end))
            k1355))))
      (fn a1362 =>
        (fn k21363 =>
          (k21363 a1362))))
    k1354);


fun report_179 ev1151_1151 ev1152_1152 prog_178 k1364 =
  ((((fn ev1154_1154 => fn ExceptionDollarcapability_1155 => fn k1371 =>
          (prog_178
            (nested ev1154_1154 ev1152_1152)
            here
            ExceptionDollarcapability_1155
            k1371))
        lift
        (Exception_164 (fn ev1153_1153 => fn exception_249 => fn msg_250 => fn k1365 =>
          ((ev1153_1153
              (fn k1366 =>
                let fun resume_251 ev1368 a1367 = (ev1368 (k1366 a1367));
                in (fn k1369 =>
                  let val tmp1102_1370 = (printlnDollar3_11 msg_250);
                  in (k1369 tmp1102_1370)
                  end)
                end))
            k1365))))
      (fn a1372 =>
        (fn k21373 =>
          (k21373 a1372))))
    k1364);


fun ignoring_182 ev1156_1156 ev1157_1157 prog_181 k1374 =
  ((((fn ev1159_1159 => fn ExceptionDollarcapability_1160 => fn k1380 =>
          (prog_181
            (nested ev1159_1159 ev1157_1157)
            here
            ExceptionDollarcapability_1160
            k1380))
        lift
        (Exception_164 (fn ev1158_1158 => fn exception_252 => fn msg_253 => fn k1375 =>
          ((ev1158_1158
              (fn k1376 =>
                let fun resume_254 ev1378 a1377 = (ev1378 (k1376 a1377));
                in (fn k1379 => (k1379 ()))
                end))
            k1375))))
      (fn a1381 =>
        (fn k21382 =>
          (k21382 a1381))))
    k1374);


fun each_186 ev1161_1161 ev1162_1162 start_183 end_184 action_185 k1383 =
  let fun loop_256 ev1163_1163 i_255 k1384 =
    (if (infixLt_93 i_255 end_184) then (action_185
      (nested ev1163_1163 ev1162_1162)
      i_255
      (fn a1385 =>
        let val f__1386 = a1385;
        in (loop_256 ev1163_1163 (infixAdd_31 i_255 1) k1384)
        end)) else (k1384 ()));
  in (loop_256 here start_183 k1383)
  end;


fun repeat_189 ev1164_1164 ev1165_1165 n_187 action_188 k1387 =
  (each_186
    ev1164_1164
    here
    0
    n_187
    (fn ev1166_1166 => fn n_257 => fn k1388 =>
      (action_188 (nested ev1166_1166 ev1165_1165) k1388))
    k1387);


fun timed_191 ev1167_1167 ev1168_1168 block_190 k1389 =
  let val before_258 =
    let val tmp1121_1390 = (timestamp_15 ());
    in tmp1121_1390
    end;
  in (block_190
    ev1168_1168
    (fn a1391 =>
      let
        val f__1392 = a1391;
        val after_259 =
          let val tmp1124_1393 = (timestamp_15 ());
          in tmp1124_1393
          end;
      in (k1389 (infixMul_34 1000 (infixSub_40 after_259 before_258)))
      end))
  end;


fun measure_195 ev1169_1169 ev1170_1170 warmup_192 iterations_193 block_194 k1394 =
  let
    fun run_262 ev1171_1171 n_260 report_261 k1395 =
      (if (infixLte_96 n_260 0) then (k1395 ()) else (timed_191
        (nested ev1171_1171 ev1169_1169)
        here
        (fn ev1172_1172 => fn k1396 =>
          (block_194
            (nested (nested ev1172_1172 ev1171_1171) ev1170_1170)
            k1396))
        (fn a1397 =>
          let
            val time_263 = a1397;
            val k1398 =
              (fn a1399 =>
                let
                  val tmp1130_1400 = a1399;
                  val _ = tmp1130_1400;
                in
                  (run_262
                    ev1171_1171
                    (infixSub_40 n_260 1)
                    report_261
                    k1395)
                end);
          in
            (if report_261 then let val tmp1129_1401 =
              (println_5 time_263);
            in (k1398 tmp1129_1401)
            end else (k1398 ()))
          end)));
    val _ = (run_262 here warmup_192 false (fn a1402 => a1402));
  in (run_262 here iterations_193 true k1394)
  end;


fun isDefined_353 ev1173_1173 self_352 k1403 =
  let
    val tmp1040_1404 = self_352;
    fun tmp1041_1174 ev1176_1176 k1405 = (k1405 false);
    fun tmp1042_1175 ev1177_1177 v_382 k1406 = (k1406 true);
  in
    (
      case tmp1040_1404 of 
        None_378 => (tmp1041_1174 here k1403)
        | Some_379 tmp1043_1407 => (tmp1042_1175 here tmp1043_1407 k1403)
      )
  end;


fun isEmpty_356 ev1178_1178 self_355 k1408 =
  let val tmp1046_1409 =
    (isDefined_353 ev1178_1178 self_355 (fn a1410 => a1410));
  in (k1408 (not_128 tmp1046_1409))
  end;


fun orElse_360 ev1179_1179 ev1180_1180 self_358 that_359 k1411 =
  let
    val tmp1047_1412 = self_358;
    fun tmp1050_1181 ev1183_1183 k1413 =
      (that_359 (nested ev1183_1183 ev1180_1180) k1413);
    fun tmp1051_1182 ev1184_1184 v_383 k1414 = (k1414 (Some_379 v_383));
  in
    (
      case tmp1047_1412 of 
        None_378 => (tmp1050_1181 here k1411)
        | Some_379 tmp1052_1415 => (tmp1051_1182 here tmp1052_1415 k1411)
      )
  end;


fun getOrElse_364 ev1185_1185 ev1186_1186 self_362 that_363 k1416 =
  let
    val tmp1055_1417 = self_362;
    fun tmp1058_1187 ev1189_1189 k1418 =
      (that_363 (nested ev1189_1189 ev1186_1186) k1418);
    fun tmp1059_1188 ev1190_1190 v_384 k1419 = (k1419 v_384);
  in
    (
      case tmp1055_1417 of 
        None_378 => (tmp1058_1187 here k1416)
        | Some_379 tmp1060_1420 => (tmp1059_1188 here tmp1060_1420 k1416)
      )
  end;


fun map_369 ev1191_1191 ev1192_1192 self_367 f_368 k1421 =
  let
    val tmp1063_1422 = self_367;
    fun tmp1064_1193 ev1195_1195 k1423 = (k1423 None_378);
    fun tmp1066_1194 ev1196_1196 v_385 k1424 =
      (f_368
        (nested ev1196_1196 ev1192_1192)
        v_385
        (fn a1425 =>
          let val tmp1065_1426 = a1425;
          in (k1424 (Some_379 tmp1065_1426))
          end));
  in
    (
      case tmp1063_1422 of 
        None_378 => (tmp1064_1193 here k1421)
        | Some_379 tmp1067_1427 => (tmp1066_1194 here tmp1067_1427 k1421)
      )
  end;


fun foreach_373 ev1197_1197 ev1198_1198 self_371 f_372 k1428 =
  let
    val tmp1070_1429 = self_371;
    fun tmp1071_1199 ev1201_1201 k1430 = (k1430 ());
    fun tmp1074_1200 ev1202_1202 v_386 k1431 =
      (f_372 (nested ev1202_1202 ev1198_1198) v_386 k1431);
  in
    (
      case tmp1070_1429 of 
        None_378 => (tmp1071_1199 here k1428)
        | Some_379 tmp1075_1432 => (tmp1074_1200 here tmp1075_1432 k1428)
      )
  end;


fun show_377 ev1203_1203 ev1204_1204 o_375 showA_376 k1433 =
  let
    val tmp1078_1434 = o_375;
    fun tmp1079_1205 ev1207_1207 k1435 = (k1435 "None()");
    fun tmp1081_1206 ev1208_1208 v_387 k1436 =
      (showA_376
        (nested ev1208_1208 ev1204_1204)
        v_387
        (fn a1437 =>
          let val tmp1080_1438 = a1437;
          in (k1436
            (infixConcat_28 (infixConcat_28 "Some(" tmp1080_1438) ")"))
          end));
  in
    (
      case tmp1078_1434 of 
        None_378 => (tmp1079_1205 here k1433)
        | Some_379 tmp1082_1439 => (tmp1081_1206 here tmp1082_1439 k1433)
      )
  end;


fun isEmpty_437 ev1209_1209 opt_436 k1440 =
  (k1440 (not_128 (isDefined_434 opt_436)));


fun toOption_443 ev1210_1210 opt_442 k1441 =
  (if (isDefined_434 opt_442) then (k1441 (Some_379 (force_440 opt_442))) else (k1441
    None_378));


fun isEmpty_464 ev1211_1211 l_463 k1442 =
  let
    val tmp859_1443 = l_463;
    fun tmp860_1212 ev1214_1214 k1444 = (k1444 true);
    fun tmp861_1213 ev1215_1215 a_523 rest_524 k1445 = (k1445 false);
  in
    (
      case tmp859_1443 of 
        Nil_517 => (tmp860_1212 here k1442)
        | Cons_518 (tmp862_865, tmp863_864) => (tmp861_1213
          here
          tmp862_865
          tmp863_864
          k1442)
      )
  end;


fun foreach_468 ev1216_1216 ev1217_1217 l_466 f_467 k1446 =
  let fun loop_526 ev1218_1218 remainder_525 k1447 =
    let val tmp868_1448 =
      (isEmpty_464
        (nested ev1218_1218 ev1216_1216)
        remainder_525
        (fn a1449 =>
          a1449));
    in (if (not_128 tmp868_1448) then let
      val tmp869_1450 = remainder_525;
      fun tmp870_1219 ev1221_1221 k1451 = (k1451 ());
      fun tmp875_1220 ev1222_1222 a_527 as_528 k1452 =
        (f_467
          (nested (nested ev1222_1222 ev1218_1218) ev1217_1217)
          a_527
          (fn a1453 =>
            let val f__1454 = a1453;
            in (loop_526 (nested ev1222_1222 ev1218_1218) as_528 k1452)
            end));
    in
      (
        case tmp869_1450 of 
          Nil_517 => (tmp870_1219 here k1447)
          | Cons_518 (tmp876_879, tmp877_878) => (tmp875_1220
            here
            tmp876_879
            tmp877_878
            k1447)
        )
    end else (k1447 ()))
    end;
  in (loop_526 here l_466 k1446)
  end;


fun size_471 ev1223_1223 l_470 k1455 =
  let fun loop_531 ev1224_1224 lst_529 acc_530 k1456 =
    let
      val tmp886_1457 = lst_529;
      fun tmp887_1225 ev1227_1227 k1458 = (k1458 acc_530);
      fun tmp890_1226 ev1228_1228 a_532 as_533 k1459 =
        (loop_531
          (nested ev1228_1228 ev1224_1224)
          as_533
          (infixAdd_31 acc_530 1)
          k1459);
    in
      (
        case tmp886_1457 of 
          Nil_517 => (tmp887_1225 here k1456)
          | Cons_518 (tmp891_894, tmp892_893) => (tmp890_1226
            here
            tmp891_894
            tmp892_893
            k1456)
        )
    end;
  in (loop_531 here l_470 0 k1455)
  end;


fun reverse_474 ev1229_1229 l_473 k1460 =
  let fun loop_536 ev1230_1230 lst_534 acc_535 k1461 =
    let
      val tmp899_1462 = lst_534;
      fun tmp900_1231 ev1233_1233 k1463 = (k1463 acc_535);
      fun tmp903_1232 ev1234_1234 a_537 as_538 k1464 =
        (loop_536
          (nested ev1234_1234 ev1230_1230)
          as_538
          (Cons_518 (a_537, acc_535))
          k1464);
    in
      (
        case tmp899_1462 of 
          Nil_517 => (tmp900_1231 here k1461)
          | Cons_518 (tmp904_907, tmp905_906) => (tmp903_1232
            here
            tmp904_907
            tmp905_906
            k1461)
        )
    end;
  in (loop_536 here l_473 Nil_517 k1460)
  end;


fun map_479 ev1235_1235 ev1236_1236 l_477 f_478 k1465 =
  let fun loop_541 ev1237_1237 lst_539 acc_540 k1466 =
    let
      val tmp912_1467 = lst_539;
      fun tmp913_1238 ev1240_1240 k1468 = (k1468 acc_540);
      fun tmp917_1239 ev1241_1241 a_542 as_543 k1469 =
        (f_478
          (nested (nested ev1241_1241 ev1237_1237) ev1236_1236)
          a_542
          (fn a1470 =>
            let val tmp914_1471 = a1470;
            in (loop_541
              (nested ev1241_1241 ev1237_1237)
              as_543
              (Cons_518 (tmp914_1471, acc_540))
              k1469)
            end));
    in
      (
        case tmp912_1467 of 
          Nil_517 => (tmp913_1238 here k1466)
          | Cons_518 (tmp918_921, tmp919_920) => (tmp917_1239
            here
            tmp918_921
            tmp919_920
            k1466)
        )
    end;
  in (loop_541
    here
    l_477
    Nil_517
    (fn a1472 =>
      let val tmp924_1473 = a1472;
      in (reverse_474 ev1235_1235 tmp924_1473 k1465)
      end))
  end;


fun reverseOnto_483 ev1242_1242 l_481 other_482 k1474 =
  let
    val tmp927_1475 = l_481;
    fun tmp928_1243 ev1245_1245 k1476 = (k1476 other_482);
    fun tmp931_1244 ev1246_1246 a_544 rest_545 k1477 =
      (reverseOnto_483
        (nested ev1246_1246 ev1242_1242)
        rest_545
        (Cons_518 (a_544, other_482))
        k1477);
  in
    (
      case tmp927_1475 of 
        Nil_517 => (tmp928_1243 here k1474)
        | Cons_518 (tmp932_935, tmp933_934) => (tmp931_1244
          here
          tmp932_935
          tmp933_934
          k1474)
      )
  end;


fun append_487 ev1247_1247 l_485 other_486 k1478 =
  let val tmp938_1479 =
    (reverse_474 ev1247_1247 l_485 (fn a1480 => a1480));
  in (reverseOnto_483 ev1247_1247 tmp938_1479 other_486 k1478)
  end;


fun take_491 ev1248_1248 l_489 n_490 k1481 =
  (if (infixEq_86 n_490 0) then (k1481 Nil_517) else let
    val tmp941_1482 = l_489;
    fun tmp942_1249 ev1251_1251 k1483 = (k1483 Nil_517);
    fun tmp944_1250 ev1252_1252 a_546 rest_547 k1484 =
      let val tmp943_1485 =
        (take_491
          (nested ev1252_1252 ev1248_1248)
          rest_547
          (infixSub_40 n_490 1)
          (fn a1486 =>
            a1486));
      in (k1484 (Cons_518 (a_546, tmp943_1485)))
      end;
  in
    (
      case tmp941_1482 of 
        Nil_517 => (tmp942_1249 here k1481)
        | Cons_518 (tmp945_948, tmp946_947) => (tmp944_1250
          here
          tmp945_948
          tmp946_947
          k1481)
      )
  end);


fun drop_495 ev1253_1253 l_493 n_494 k1487 =
  (if (infixEq_86 n_494 0) then (k1487 l_493) else let
    val tmp953_1488 = l_493;
    fun tmp954_1254 ev1256_1256 k1489 = (k1489 Nil_517);
    fun tmp957_1255 ev1257_1257 a_548 rest_549 k1490 =
      (drop_495
        (nested ev1257_1257 ev1253_1253)
        rest_549
        (infixSub_40 n_494 1)
        k1490);
  in
    (
      case tmp953_1488 of 
        Nil_517 => (tmp954_1254 here k1487)
        | Cons_518 (tmp958_961, tmp959_960) => (tmp957_1255
          here
          tmp958_961
          tmp959_960
          k1487)
      )
  end);


fun nonEmpty_498 ev1258_1258 l_497 k1491 =
  let
    val tmp966_1492 = l_497;
    fun tmp967_1259 ev1261_1261 k1493 = (k1493 false);
    fun tmp968_1260 ev1262_1262 a_550 rest_551 k1494 = (k1494 true);
  in
    (
      case tmp966_1492 of 
        Nil_517 => (tmp967_1259 here k1491)
        | Cons_518 (tmp969_972, tmp970_971) => (tmp968_1260
          here
          tmp969_972
          tmp970_971
          k1491)
      )
  end;


fun head_501 ev1263_1263 ev1264_1264 l_500 ExceptionDollarcapability_1265 k1495 =
  let
    val tmp975_1496 = l_500;
    fun tmp978_1266 ev1268_1268 k1497 =
      ((raise_244 ExceptionDollarcapability_1265)
        (nested ev1268_1268 ev1264_1264)
        EmptyList_516
        "Trying to get the head of an empty list"
        k1497);
    fun tmp979_1267 ev1269_1269 a_552 rest_553 k1498 = (k1498 a_552);
  in
    (
      case tmp975_1496 of 
        Nil_517 => (tmp978_1266 here k1495)
        | Cons_518 (tmp980_983, tmp981_982) => (tmp979_1267
          here
          tmp980_983
          tmp981_982
          k1495)
      )
  end;


fun tail_504 ev1270_1270 ev1271_1271 l_503 ExceptionDollarcapability_1272 k1499 =
  let
    val tmp986_1500 = l_503;
    fun tmp989_1273 ev1275_1275 k1501 =
      ((raise_244 ExceptionDollarcapability_1272)
        (nested ev1275_1275 ev1271_1271)
        EmptyList_516
        "Trying to get the head of an empty list"
        k1501);
    fun tmp990_1274 ev1276_1276 a_554 rest_555 k1502 = (k1502 rest_555);
  in
    (
      case tmp986_1500 of 
        Nil_517 => (tmp989_1273 here k1499)
        | Cons_518 (tmp991_994, tmp992_993) => (tmp990_1274
          here
          tmp991_994
          tmp992_993
          k1499)
      )
  end;


fun headOption_507 ev1277_1277 l_506 k1503 =
  let
    val tmp997_1504 = l_506;
    fun tmp998_1278 ev1280_1280 k1505 = (k1505 None_378);
    fun tmp999_1279 ev1281_1281 a_556 rest_557 k1506 =
      (k1506 (Some_379 a_556));
  in
    (
      case tmp997_1504 of 
        Nil_517 => (tmp998_1278 here k1503)
        | Cons_518 (tmp1000_1003, tmp1001_1002) => (tmp999_1279
          here
          tmp1000_1003
          tmp1001_1002
          k1503)
      )
  end;


fun partition_511 ev1282_1282 ev1283_1283 l_509 pred_510 k1507 =
  ((withRegion
      (fn ev1284_1284 => fn this_1023 => fn k1508 =>
        let
          val tmp1006_1509 = Nil_517;
          val lefts_558 = (fresh this_1023 tmp1006_1509);
          val tmp1007_1511 = Nil_517;
          val rights_559 = (fresh this_1023 tmp1007_1511);
        in
          (foreach_468
            (nested ev1284_1284 ev1282_1282)
            here
            l_509
            (fn ev1285_1285 => fn el_560 => fn k1512 =>
              (pred_510
                (nested (nested ev1285_1285 ev1284_1284) ev1283_1283)
                el_560
                (fn a1513 =>
                  let val tmp1008_1514 = a1513;
                  in (if tmp1008_1514 then let val tmp1009_1515 =
                    (!lefts_558);
                  in (k1512
                    (lefts_558 := (Cons_518 (el_560, tmp1009_1515))))
                  end else let val tmp1012_1516 = (!rights_559);
                  in (k1512
                    (rights_559 := (Cons_518 (el_560, tmp1012_1516))))
                  end)
                  end)))
            (fn a1517 =>
              let
                val f__1518 = a1517;
                val tmp1019_1519 = (!lefts_558);
                val tmp1020_1520 =
                  (reverse_474
                    (nested ev1284_1284 ev1282_1282)
                    tmp1019_1519
                    (fn a1521 =>
                      a1521));
                val tmp1021_1522 = (!rights_559);
                val tmp1022_1523 =
                  (reverse_474
                    (nested ev1284_1284 ev1282_1282)
                    tmp1021_1522
                    (fn a1524 =>
                      a1524));
              in (k1508 (Tuple2_197 (tmp1020_1520, tmp1022_1523)))
              end))
        end))
    k1507);


fun show_515 ev1286_1286 ev1287_1287 l_513 showA_514 k1525 =
  let
    val tmp1026_1526 = l_513;
    fun tmp1027_1288 ev1290_1290 k1527 = (k1527 "Nil()");
    fun tmp1031_1289 ev1291_1291 x_561 xs_562 k1528 =
      (showA_514
        (nested ev1291_1291 ev1287_1287)
        x_561
        (fn a1529 =>
          let val tmp1028_1530 = a1529;
          in (show_515
            (nested ev1291_1291 ev1286_1286)
            (nested ev1291_1291 ev1287_1287)
            xs_562
            showA_514
            (fn a1531 =>
              let val tmp1030_1532 = a1531;
              in (k1528
                (infixConcat_28
                  (infixConcat_28
                    (infixConcat_28
                      (infixConcat_28 "Cons(" tmp1028_1530)
                      ", ")
                    tmp1030_1532)
                  ")"))
              end))
          end));
  in
    (
      case tmp1026_1526 of 
        Nil_517 => (tmp1027_1288 here k1525)
        | Cons_518 (tmp1032_1035, tmp1033_1034) => (tmp1031_1289
          here
          tmp1032_1035
          tmp1033_1034
          k1525)
      )
  end;


fun charAt_701 ev1292_1292 str_700 index_699 k1533 =
  (if (infixOr_131
    (infixLt_93 index_699 0)
    (infixLte_96 (length_703 str_700) index_699)) then (k1533
    (Some_379 (unsafeCharAt_722 str_700 index_699))) else (k1533 None_378));


fun substring_712 ev1293_1293 str_710 from_711 k1534 =
  (if (infixOr_131
    (infixLt_93 from_711 0)
    (infixLte_96 (length_703 str_710) from_711)) then (k1534 str_710) else (k1534
    (unsafeSubstring_715 str_710 from_711)));


fun toInt_719 ev1294_1294 str_718 k1535 =
  (toOption_443 ev1294_1294 (internalStringToInt_717 str_718) k1535);


fun triple_742 ev1295_1295 ev1296_1296 ev1298_1298 ev1300_1300 n_740 s_741 FailDollarcapability_1297 ChooseDollarcapability_1299 YieldDollarcapability_1301 k1536 =
  ((choose_751 ChooseDollarcapability_1299)
    ev1298_1298
    n_740
    (fn a1537 =>
      let val i_757 = a1537;
      in ((choose_751 ChooseDollarcapability_1299)
        ev1298_1298
        (infixSub_40 i_757 1)
        (fn a1538 =>
          let val j_758 = a1538;
          in ((choose_751 ChooseDollarcapability_1299)
            ev1298_1298
            (infixSub_40 j_758 1)
            (fn a1539 =>
              let val k_759 = a1539;
              in (if (infixEq_86
                (infixAdd_31 (infixAdd_31 i_757 j_758) k_759)
                s_741) then ((yield_756 YieldDollarcapability_1301)
                ev1300_1300
                i_757
                j_758
                k_759
                k1536) else ((fail_752 FailDollarcapability_1297)
                ev1296_1296
                k1536))
              end))
          end))
      end));


fun yieldTriples_745 ev1302_1302 ev1303_1303 n_743 s_744 YieldDollarcapability_1304 k1540 =
  ((withRegion
      (fn ev1305_1305 => fn this_833 => fn k1541 =>
        ((((fn ev1310_1310 => fn ChooseDollarcapability_1311 => fn FailDollarcapability_1312 => fn k1560 =>
                (triple_742
                  (nested (nested ev1310_1310 ev1305_1305) ev1302_1302)
                  here
                  here
                  (nested (nested ev1310_1310 ev1305_1305) ev1303_1303)
                  n_743
                  s_744
                  FailDollarcapability_1312
                  ChooseDollarcapability_1311
                  YieldDollarcapability_1304
                  k1560))
              lift
              (Choose_737 (fn ev1306_1306 => fn n_760 => fn k1542 =>
                ((ev1306_1306
                    (fn k1543 =>
                      let fun resume_761 ev1545 a1544 =
                        (ev1545 (k1543 a1544));
                      in (fn k1546 =>
                        let
                          val tmp821_1547 = 1;
                          val i_762 = (fresh this_833 tmp821_1547);
                          fun tmp822_1307 ev1308_1308 k1548 =
                            let val tmp823_1549 = (!i_762);
                            in (if (infixLte_96 tmp823_1549 n_760) then let val tmp825_1550 =
                              (!i_762);
                            in (resume_761
                              here
                              tmp825_1550
                              (fn a1551 =>
                                let
                                  val f__1552 = a1551;
                                  val tmp828_1553 = (!i_762);
                                  val tmp824_1554 =
                                    (i_762 := (infixAdd_31 tmp828_1553 1));
                                in (tmp822_1307 ev1308_1308 k1548)
                                end))
                            end else (k1548 ()))
                            end;
                        in (tmp822_1307 here k1546)
                        end)
                      end))
                  k1542)))
              (Fail_736 (fn ev1309_1309 => fn k1555 =>
                ((ev1309_1309
                    (fn k1556 =>
                      let fun resume_763 ev1558 a1557 =
                        (ev1558 (k1556 a1557));
                      in (fn k1559 => (k1559 ()))
                      end))
                  k1555))))
            (fn a1561 =>
              (fn k21562 =>
                (k21562 a1561))))
          k1541)))
    k1540);


fun countTriples_748 ev1313_1313 n_746 s_747 k1563 =
  ((withRegion
      (fn ev1314_1314 => fn this_848 => fn k1564 =>
        let
          val tmp838_1565 = 0;
          val cnt_764 = (fresh this_848 tmp838_1565);
        in
          ((((fn ev1316_1316 => fn YieldDollarcapability_1317 => fn k1573 =>
                  (yieldTriples_745
                    (nested (nested ev1316_1316 ev1314_1314) ev1313_1313)
                    here
                    n_746
                    s_747
                    YieldDollarcapability_1317
                    k1573))
                lift
                (Yield_739 (fn ev1315_1315 => fn i_765 => fn j_766 => fn k_767 => fn k1566 =>
                  ((ev1315_1315
                      (fn k1567 =>
                        let fun resume_768 ev1569 a1568 =
                          (ev1569 (k1567 a1568));
                        in (fn k1570 =>
                          let
                            val tmp841_1571 = (!cnt_764);
                            val tmp842_1572 =
                              (cnt_764 := (infixAdd_31 tmp841_1571 1));
                            val _ = tmp842_1572;
                          in (resume_768 here () k1570)
                          end)
                        end))
                    k1566))))
              (fn a1574 =>
                (fn k21575 =>
                  (k21575 a1574))))
            (fn a1576 =>
              let
                val tmp845_1577 = a1576;
                val _ = tmp845_1577;
              in (k1564 (!cnt_764))
              end))
        end))
    k1563);


fun main_749 ev1318_1318 k1578 =
  let
    val tmp851_1579 =
      (countTriples_748 ev1318_1318 500 127 (fn a1580 => a1580));
    val tmp852_1581 = (println_5 tmp851_1579);
  in (k1578 tmp852_1581)
  end;

(main_749 (fn a => a) (fn a => a));
