datatype ('arg1309, 'arg1310) Tuple2_137 =
  Tuple2_197 of ('arg1309 * 'arg1310);


fun first_200 (Tuple2_197 (first_200, _)) = first_200;


fun second_201 (Tuple2_197 (_, second_201)) = second_201;


datatype ('arg1311, 'arg1312, 'arg1313) Tuple3_141 =
  Tuple3_202 of ('arg1311 * 'arg1312 * 'arg1313);


fun firstDollar1_206 (Tuple3_202 (firstDollar1_206, _, _)) =
  firstDollar1_206;


fun secondDollar1_207 (Tuple3_202 (_, secondDollar1_207, _)) =
  secondDollar1_207;


fun third_208 (Tuple3_202 (_, _, third_208)) = third_208;


datatype ('arg1314, 'arg1315, 'arg1316, 'arg1317) Tuple4_146 =
  Tuple4_209 of ('arg1314 * 'arg1315 * 'arg1316 * 'arg1317);


fun firstDollar2_214 (Tuple4_209 (firstDollar2_214, _, _, _)) =
  firstDollar2_214;


fun secondDollar2_215 (Tuple4_209 (_, secondDollar2_215, _, _)) =
  secondDollar2_215;


fun thirdDollar1_216 (Tuple4_209 (_, _, thirdDollar1_216, _)) =
  thirdDollar1_216;


fun fourth_217 (Tuple4_209 (_, _, _, fourth_217)) = fourth_217;


datatype ('arg1318, 'arg1319, 'arg1320, 'arg1321, 'arg1322) Tuple5_152 =
  Tuple5_218 of ('arg1318 * 'arg1319 * 'arg1320 * 'arg1321 * 'arg1322);


fun firstDollar3_224 (Tuple5_218 (firstDollar3_224, _, _, _, _)) =
  firstDollar3_224;


fun secondDollar3_225 (Tuple5_218 (_, secondDollar3_225, _, _, _)) =
  secondDollar3_225;


fun thirdDollar2_226 (Tuple5_218 (_, _, thirdDollar2_226, _, _)) =
  thirdDollar2_226;


fun fourthDollar1_227 (Tuple5_218 (_, _, _, fourthDollar1_227, _)) =
  fourthDollar1_227;


fun fifth_228 (Tuple5_218 (_, _, _, _, fifth_228)) = fifth_228;


datatype ('arg1323, 'arg1324, 'arg1325, 'arg1326, 'arg1327, 'arg1328) Tuple6_159 =
  Tuple6_229 of ('arg1323 * 'arg1324 * 'arg1325 * 'arg1326 * 'arg1327 * 'arg1328);


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


datatype 'arg1329 Exception_164 =
  Exception_164 of 'arg1329;


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


datatype 'arg1330 Choose_737 =
  Choose_737 of 'arg1330;


fun Choose_749 (Choose_737 Choose_749) = Choose_749;


datatype 'arg1331 Fail_736 =
  Fail_736 of 'arg1331;


fun Fail_750 (Fail_736 Fail_750) = Fail_750;


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


fun locally_3 ev1131_1131 ev1132_1132 f_1 k1332 = (f_1 ev1132_1132 k1332);


val PI_196 = (f_pi_74 ());


fun raise_168 ev1133_1133 ev1134_1134 msg_167 ExceptionDollarcapability_1135 k1333 =
  ((raise_244 ExceptionDollarcapability_1135)
    ev1134_1134
    RuntimeError_245
    msg_167
    (fn a1336 =>
      let
        val tmp1081_1337 = a1336;
        val tmp1082_1338 = tmp1081_1337;
      in (raise Hole)
      end));


fun raiseDollar1_173 ev1137_1137 ev1138_1138 exception_171 msg_172 ExceptionDollarcapability_1139 k1339 =
  ((raise_244 ExceptionDollarcapability_1139)
    ev1138_1138
    exception_171
    msg_172
    (fn a1340 =>
      let
        val tmp1085_1341 = a1340;
        val tmp1086_1342 = tmp1085_1341;
      in (raise Hole)
      end));


fun panicOn_176 ev1140_1140 ev1141_1141 prog_175 k1343 =
  ((((fn ev1143_1143 => fn ExceptionDollarcapability_1144 => fn k1350 =>
          (prog_175
            (nested ev1143_1143 ev1141_1141)
            here
            ExceptionDollarcapability_1144
            k1350))
        lift
        (Exception_164 (fn ev1142_1142 => fn exception_246 => fn msg_247 => fn k1344 =>
          ((ev1142_1142
              (fn k1345 =>
                let fun resume_248 ev1347 a1346 = (ev1347 (k1345 a1346));
                in (fn k1348 =>
                  let val tmp1091_1349 = (panic_162 msg_247);
                  in (k1348 tmp1091_1349)
                  end)
                end))
            k1344))))
      (fn a1351 =>
        (fn k21352 =>
          (k21352 a1351))))
    k1343);


fun report_179 ev1145_1145 ev1146_1146 prog_178 k1353 =
  ((((fn ev1148_1148 => fn ExceptionDollarcapability_1149 => fn k1360 =>
          (prog_178
            (nested ev1148_1148 ev1146_1146)
            here
            ExceptionDollarcapability_1149
            k1360))
        lift
        (Exception_164 (fn ev1147_1147 => fn exception_249 => fn msg_250 => fn k1354 =>
          ((ev1147_1147
              (fn k1355 =>
                let fun resume_251 ev1357 a1356 = (ev1357 (k1355 a1356));
                in (fn k1358 =>
                  let val tmp1096_1359 = (printlnDollar3_11 msg_250);
                  in (k1358 tmp1096_1359)
                  end)
                end))
            k1354))))
      (fn a1361 =>
        (fn k21362 =>
          (k21362 a1361))))
    k1353);


fun ignoring_182 ev1150_1150 ev1151_1151 prog_181 k1363 =
  ((((fn ev1153_1153 => fn ExceptionDollarcapability_1154 => fn k1369 =>
          (prog_181
            (nested ev1153_1153 ev1151_1151)
            here
            ExceptionDollarcapability_1154
            k1369))
        lift
        (Exception_164 (fn ev1152_1152 => fn exception_252 => fn msg_253 => fn k1364 =>
          ((ev1152_1152
              (fn k1365 =>
                let fun resume_254 ev1367 a1366 = (ev1367 (k1365 a1366));
                in (fn k1368 => (k1368 ()))
                end))
            k1364))))
      (fn a1370 =>
        (fn k21371 =>
          (k21371 a1370))))
    k1363);


fun each_186 ev1155_1155 ev1156_1156 start_183 end_184 action_185 k1372 =
  let fun loop_256 ev1157_1157 i_255 k1373 =
    (if (infixLt_93 i_255 end_184) then (action_185
      (nested ev1157_1157 ev1156_1156)
      i_255
      (fn a1374 =>
        let val f__1375 = a1374;
        in (loop_256 ev1157_1157 (infixAdd_31 i_255 1) k1373)
        end)) else (k1373 ()));
  in (loop_256 here start_183 k1372)
  end;


fun repeat_189 ev1158_1158 ev1159_1159 n_187 action_188 k1376 =
  (each_186
    ev1158_1158
    here
    0
    n_187
    (fn ev1160_1160 => fn n_257 => fn k1377 =>
      (action_188 (nested ev1160_1160 ev1159_1159) k1377))
    k1376);


fun timed_191 ev1161_1161 ev1162_1162 block_190 k1378 =
  let val before_258 =
    let val tmp1115_1379 = (timestamp_15 ());
    in tmp1115_1379
    end;
  in (block_190
    ev1162_1162
    (fn a1380 =>
      let
        val f__1381 = a1380;
        val after_259 =
          let val tmp1118_1382 = (timestamp_15 ());
          in tmp1118_1382
          end;
      in (k1378 (infixMul_34 1000 (infixSub_40 after_259 before_258)))
      end))
  end;


fun measure_195 ev1163_1163 ev1164_1164 warmup_192 iterations_193 block_194 k1383 =
  let
    fun run_262 ev1165_1165 n_260 report_261 k1384 =
      (if (infixLte_96 n_260 0) then (k1384 ()) else (timed_191
        (nested ev1165_1165 ev1163_1163)
        here
        (fn ev1166_1166 => fn k1385 =>
          (block_194
            (nested (nested ev1166_1166 ev1165_1165) ev1164_1164)
            k1385))
        (fn a1386 =>
          let
            val time_263 = a1386;
            val k1387 =
              (fn a1388 =>
                let
                  val tmp1124_1389 = a1388;
                  val _ = tmp1124_1389;
                in
                  (run_262
                    ev1165_1165
                    (infixSub_40 n_260 1)
                    report_261
                    k1384)
                end);
          in
            (if report_261 then let val tmp1123_1390 =
              (println_5 time_263);
            in (k1387 tmp1123_1390)
            end else (k1387 ()))
          end)));
    val _ = (run_262 here warmup_192 false (fn a1391 => a1391));
  in (run_262 here iterations_193 true k1383)
  end;


fun isDefined_353 ev1167_1167 self_352 k1392 =
  let
    val tmp1034_1393 = self_352;
    fun tmp1035_1168 ev1170_1170 k1394 = (k1394 false);
    fun tmp1036_1169 ev1171_1171 v_382 k1395 = (k1395 true);
  in
    (
      case tmp1034_1393 of 
        None_378 => (tmp1035_1168 here k1392)
        | Some_379 tmp1037_1396 => (tmp1036_1169 here tmp1037_1396 k1392)
      )
  end;


fun isEmpty_356 ev1172_1172 self_355 k1397 =
  let val tmp1040_1398 =
    (isDefined_353 ev1172_1172 self_355 (fn a1399 => a1399));
  in (k1397 (not_128 tmp1040_1398))
  end;


fun orElse_360 ev1173_1173 ev1174_1174 self_358 that_359 k1400 =
  let
    val tmp1041_1401 = self_358;
    fun tmp1044_1175 ev1177_1177 k1402 =
      (that_359 (nested ev1177_1177 ev1174_1174) k1402);
    fun tmp1045_1176 ev1178_1178 v_383 k1403 = (k1403 (Some_379 v_383));
  in
    (
      case tmp1041_1401 of 
        None_378 => (tmp1044_1175 here k1400)
        | Some_379 tmp1046_1404 => (tmp1045_1176 here tmp1046_1404 k1400)
      )
  end;


fun getOrElse_364 ev1179_1179 ev1180_1180 self_362 that_363 k1405 =
  let
    val tmp1049_1406 = self_362;
    fun tmp1052_1181 ev1183_1183 k1407 =
      (that_363 (nested ev1183_1183 ev1180_1180) k1407);
    fun tmp1053_1182 ev1184_1184 v_384 k1408 = (k1408 v_384);
  in
    (
      case tmp1049_1406 of 
        None_378 => (tmp1052_1181 here k1405)
        | Some_379 tmp1054_1409 => (tmp1053_1182 here tmp1054_1409 k1405)
      )
  end;


fun map_369 ev1185_1185 ev1186_1186 self_367 f_368 k1410 =
  let
    val tmp1057_1411 = self_367;
    fun tmp1058_1187 ev1189_1189 k1412 = (k1412 None_378);
    fun tmp1060_1188 ev1190_1190 v_385 k1413 =
      (f_368
        (nested ev1190_1190 ev1186_1186)
        v_385
        (fn a1414 =>
          let val tmp1059_1415 = a1414;
          in (k1413 (Some_379 tmp1059_1415))
          end));
  in
    (
      case tmp1057_1411 of 
        None_378 => (tmp1058_1187 here k1410)
        | Some_379 tmp1061_1416 => (tmp1060_1188 here tmp1061_1416 k1410)
      )
  end;


fun foreach_373 ev1191_1191 ev1192_1192 self_371 f_372 k1417 =
  let
    val tmp1064_1418 = self_371;
    fun tmp1065_1193 ev1195_1195 k1419 = (k1419 ());
    fun tmp1068_1194 ev1196_1196 v_386 k1420 =
      (f_372 (nested ev1196_1196 ev1192_1192) v_386 k1420);
  in
    (
      case tmp1064_1418 of 
        None_378 => (tmp1065_1193 here k1417)
        | Some_379 tmp1069_1421 => (tmp1068_1194 here tmp1069_1421 k1417)
      )
  end;


fun show_377 ev1197_1197 ev1198_1198 o_375 showA_376 k1422 =
  let
    val tmp1072_1423 = o_375;
    fun tmp1073_1199 ev1201_1201 k1424 = (k1424 "None()");
    fun tmp1075_1200 ev1202_1202 v_387 k1425 =
      (showA_376
        (nested ev1202_1202 ev1198_1198)
        v_387
        (fn a1426 =>
          let val tmp1074_1427 = a1426;
          in (k1425
            (infixConcat_28 (infixConcat_28 "Some(" tmp1074_1427) ")"))
          end));
  in
    (
      case tmp1072_1423 of 
        None_378 => (tmp1073_1199 here k1422)
        | Some_379 tmp1076_1428 => (tmp1075_1200 here tmp1076_1428 k1422)
      )
  end;


fun isEmpty_437 ev1203_1203 opt_436 k1429 =
  (k1429 (not_128 (isDefined_434 opt_436)));


fun toOption_443 ev1204_1204 opt_442 k1430 =
  (if (isDefined_434 opt_442) then (k1430 (Some_379 (force_440 opt_442))) else (k1430
    None_378));


fun isEmpty_464 ev1205_1205 l_463 k1431 =
  let
    val tmp853_1432 = l_463;
    fun tmp854_1206 ev1208_1208 k1433 = (k1433 true);
    fun tmp855_1207 ev1209_1209 a_523 rest_524 k1434 = (k1434 false);
  in
    (
      case tmp853_1432 of 
        Nil_517 => (tmp854_1206 here k1431)
        | Cons_518 (tmp856_859, tmp857_858) => (tmp855_1207
          here
          tmp856_859
          tmp857_858
          k1431)
      )
  end;


fun foreach_468 ev1210_1210 ev1211_1211 l_466 f_467 k1435 =
  let fun loop_526 ev1212_1212 remainder_525 k1436 =
    let val tmp862_1437 =
      (isEmpty_464
        (nested ev1212_1212 ev1210_1210)
        remainder_525
        (fn a1438 =>
          a1438));
    in (if (not_128 tmp862_1437) then let
      val tmp863_1439 = remainder_525;
      fun tmp864_1213 ev1215_1215 k1440 = (k1440 ());
      fun tmp869_1214 ev1216_1216 a_527 as_528 k1441 =
        (f_467
          (nested (nested ev1216_1216 ev1212_1212) ev1211_1211)
          a_527
          (fn a1442 =>
            let val f__1443 = a1442;
            in (loop_526 (nested ev1216_1216 ev1212_1212) as_528 k1441)
            end));
    in
      (
        case tmp863_1439 of 
          Nil_517 => (tmp864_1213 here k1436)
          | Cons_518 (tmp870_873, tmp871_872) => (tmp869_1214
            here
            tmp870_873
            tmp871_872
            k1436)
        )
    end else (k1436 ()))
    end;
  in (loop_526 here l_466 k1435)
  end;


fun size_471 ev1217_1217 l_470 k1444 =
  let fun loop_531 ev1218_1218 lst_529 acc_530 k1445 =
    let
      val tmp880_1446 = lst_529;
      fun tmp881_1219 ev1221_1221 k1447 = (k1447 acc_530);
      fun tmp884_1220 ev1222_1222 a_532 as_533 k1448 =
        (loop_531
          (nested ev1222_1222 ev1218_1218)
          as_533
          (infixAdd_31 acc_530 1)
          k1448);
    in
      (
        case tmp880_1446 of 
          Nil_517 => (tmp881_1219 here k1445)
          | Cons_518 (tmp885_888, tmp886_887) => (tmp884_1220
            here
            tmp885_888
            tmp886_887
            k1445)
        )
    end;
  in (loop_531 here l_470 0 k1444)
  end;


fun reverse_474 ev1223_1223 l_473 k1449 =
  let fun loop_536 ev1224_1224 lst_534 acc_535 k1450 =
    let
      val tmp893_1451 = lst_534;
      fun tmp894_1225 ev1227_1227 k1452 = (k1452 acc_535);
      fun tmp897_1226 ev1228_1228 a_537 as_538 k1453 =
        (loop_536
          (nested ev1228_1228 ev1224_1224)
          as_538
          (Cons_518 (a_537, acc_535))
          k1453);
    in
      (
        case tmp893_1451 of 
          Nil_517 => (tmp894_1225 here k1450)
          | Cons_518 (tmp898_901, tmp899_900) => (tmp897_1226
            here
            tmp898_901
            tmp899_900
            k1450)
        )
    end;
  in (loop_536 here l_473 Nil_517 k1449)
  end;


fun map_479 ev1229_1229 ev1230_1230 l_477 f_478 k1454 =
  let fun loop_541 ev1231_1231 lst_539 acc_540 k1455 =
    let
      val tmp906_1456 = lst_539;
      fun tmp907_1232 ev1234_1234 k1457 = (k1457 acc_540);
      fun tmp911_1233 ev1235_1235 a_542 as_543 k1458 =
        (f_478
          (nested (nested ev1235_1235 ev1231_1231) ev1230_1230)
          a_542
          (fn a1459 =>
            let val tmp908_1460 = a1459;
            in (loop_541
              (nested ev1235_1235 ev1231_1231)
              as_543
              (Cons_518 (tmp908_1460, acc_540))
              k1458)
            end));
    in
      (
        case tmp906_1456 of 
          Nil_517 => (tmp907_1232 here k1455)
          | Cons_518 (tmp912_915, tmp913_914) => (tmp911_1233
            here
            tmp912_915
            tmp913_914
            k1455)
        )
    end;
  in (loop_541
    here
    l_477
    Nil_517
    (fn a1461 =>
      let val tmp918_1462 = a1461;
      in (reverse_474 ev1229_1229 tmp918_1462 k1454)
      end))
  end;


fun reverseOnto_483 ev1236_1236 l_481 other_482 k1463 =
  let
    val tmp921_1464 = l_481;
    fun tmp922_1237 ev1239_1239 k1465 = (k1465 other_482);
    fun tmp925_1238 ev1240_1240 a_544 rest_545 k1466 =
      (reverseOnto_483
        (nested ev1240_1240 ev1236_1236)
        rest_545
        (Cons_518 (a_544, other_482))
        k1466);
  in
    (
      case tmp921_1464 of 
        Nil_517 => (tmp922_1237 here k1463)
        | Cons_518 (tmp926_929, tmp927_928) => (tmp925_1238
          here
          tmp926_929
          tmp927_928
          k1463)
      )
  end;


fun append_487 ev1241_1241 l_485 other_486 k1467 =
  let val tmp932_1468 =
    (reverse_474 ev1241_1241 l_485 (fn a1469 => a1469));
  in (reverseOnto_483 ev1241_1241 tmp932_1468 other_486 k1467)
  end;


fun take_491 ev1242_1242 l_489 n_490 k1470 =
  (if (infixEq_86 n_490 0) then (k1470 Nil_517) else let
    val tmp935_1471 = l_489;
    fun tmp936_1243 ev1245_1245 k1472 = (k1472 Nil_517);
    fun tmp938_1244 ev1246_1246 a_546 rest_547 k1473 =
      let val tmp937_1474 =
        (take_491
          (nested ev1246_1246 ev1242_1242)
          rest_547
          (infixSub_40 n_490 1)
          (fn a1475 =>
            a1475));
      in (k1473 (Cons_518 (a_546, tmp937_1474)))
      end;
  in
    (
      case tmp935_1471 of 
        Nil_517 => (tmp936_1243 here k1470)
        | Cons_518 (tmp939_942, tmp940_941) => (tmp938_1244
          here
          tmp939_942
          tmp940_941
          k1470)
      )
  end);


fun drop_495 ev1247_1247 l_493 n_494 k1476 =
  (if (infixEq_86 n_494 0) then (k1476 l_493) else let
    val tmp947_1477 = l_493;
    fun tmp948_1248 ev1250_1250 k1478 = (k1478 Nil_517);
    fun tmp951_1249 ev1251_1251 a_548 rest_549 k1479 =
      (drop_495
        (nested ev1251_1251 ev1247_1247)
        rest_549
        (infixSub_40 n_494 1)
        k1479);
  in
    (
      case tmp947_1477 of 
        Nil_517 => (tmp948_1248 here k1476)
        | Cons_518 (tmp952_955, tmp953_954) => (tmp951_1249
          here
          tmp952_955
          tmp953_954
          k1476)
      )
  end);


fun nonEmpty_498 ev1252_1252 l_497 k1480 =
  let
    val tmp960_1481 = l_497;
    fun tmp961_1253 ev1255_1255 k1482 = (k1482 false);
    fun tmp962_1254 ev1256_1256 a_550 rest_551 k1483 = (k1483 true);
  in
    (
      case tmp960_1481 of 
        Nil_517 => (tmp961_1253 here k1480)
        | Cons_518 (tmp963_966, tmp964_965) => (tmp962_1254
          here
          tmp963_966
          tmp964_965
          k1480)
      )
  end;


fun head_501 ev1257_1257 ev1258_1258 l_500 ExceptionDollarcapability_1259 k1484 =
  let
    val tmp969_1485 = l_500;
    fun tmp972_1260 ev1262_1262 k1486 =
      ((raise_244 ExceptionDollarcapability_1259)
        (nested ev1262_1262 ev1258_1258)
        EmptyList_516
        "Trying to get the head of an empty list"
        k1486);
    fun tmp973_1261 ev1263_1263 a_552 rest_553 k1487 = (k1487 a_552);
  in
    (
      case tmp969_1485 of 
        Nil_517 => (tmp972_1260 here k1484)
        | Cons_518 (tmp974_977, tmp975_976) => (tmp973_1261
          here
          tmp974_977
          tmp975_976
          k1484)
      )
  end;


fun tail_504 ev1264_1264 ev1265_1265 l_503 ExceptionDollarcapability_1266 k1488 =
  let
    val tmp980_1489 = l_503;
    fun tmp983_1267 ev1269_1269 k1490 =
      ((raise_244 ExceptionDollarcapability_1266)
        (nested ev1269_1269 ev1265_1265)
        EmptyList_516
        "Trying to get the head of an empty list"
        k1490);
    fun tmp984_1268 ev1270_1270 a_554 rest_555 k1491 = (k1491 rest_555);
  in
    (
      case tmp980_1489 of 
        Nil_517 => (tmp983_1267 here k1488)
        | Cons_518 (tmp985_988, tmp986_987) => (tmp984_1268
          here
          tmp985_988
          tmp986_987
          k1488)
      )
  end;


fun headOption_507 ev1271_1271 l_506 k1492 =
  let
    val tmp991_1493 = l_506;
    fun tmp992_1272 ev1274_1274 k1494 = (k1494 None_378);
    fun tmp993_1273 ev1275_1275 a_556 rest_557 k1495 =
      (k1495 (Some_379 a_556));
  in
    (
      case tmp991_1493 of 
        Nil_517 => (tmp992_1272 here k1492)
        | Cons_518 (tmp994_997, tmp995_996) => (tmp993_1273
          here
          tmp994_997
          tmp995_996
          k1492)
      )
  end;


fun partition_511 ev1276_1276 ev1277_1277 l_509 pred_510 k1496 =
  ((withRegion
      (fn ev1278_1278 => fn this_1017 => fn k1497 =>
        let
          val tmp1000_1498 = Nil_517;
          val lefts_558 = (fresh this_1017 tmp1000_1498);
          val tmp1001_1500 = Nil_517;
          val rights_559 = (fresh this_1017 tmp1001_1500);
        in
          (foreach_468
            (nested ev1278_1278 ev1276_1276)
            here
            l_509
            (fn ev1279_1279 => fn el_560 => fn k1501 =>
              (pred_510
                (nested (nested ev1279_1279 ev1278_1278) ev1277_1277)
                el_560
                (fn a1502 =>
                  let val tmp1002_1503 = a1502;
                  in (if tmp1002_1503 then let val tmp1003_1504 =
                    (!lefts_558);
                  in (k1501
                    (lefts_558 := (Cons_518 (el_560, tmp1003_1504))))
                  end else let val tmp1006_1505 = (!rights_559);
                  in (k1501
                    (rights_559 := (Cons_518 (el_560, tmp1006_1505))))
                  end)
                  end)))
            (fn a1506 =>
              let
                val f__1507 = a1506;
                val tmp1013_1508 = (!lefts_558);
                val tmp1014_1509 =
                  (reverse_474
                    (nested ev1278_1278 ev1276_1276)
                    tmp1013_1508
                    (fn a1510 =>
                      a1510));
                val tmp1015_1511 = (!rights_559);
                val tmp1016_1512 =
                  (reverse_474
                    (nested ev1278_1278 ev1276_1276)
                    tmp1015_1511
                    (fn a1513 =>
                      a1513));
              in (k1497 (Tuple2_197 (tmp1014_1509, tmp1016_1512)))
              end))
        end))
    k1496);


fun show_515 ev1280_1280 ev1281_1281 l_513 showA_514 k1514 =
  let
    val tmp1020_1515 = l_513;
    fun tmp1021_1282 ev1284_1284 k1516 = (k1516 "Nil()");
    fun tmp1025_1283 ev1285_1285 x_561 xs_562 k1517 =
      (showA_514
        (nested ev1285_1285 ev1281_1281)
        x_561
        (fn a1518 =>
          let val tmp1022_1519 = a1518;
          in (show_515
            (nested ev1285_1285 ev1280_1280)
            (nested ev1285_1285 ev1281_1281)
            xs_562
            showA_514
            (fn a1520 =>
              let val tmp1024_1521 = a1520;
              in (k1517
                (infixConcat_28
                  (infixConcat_28
                    (infixConcat_28
                      (infixConcat_28 "Cons(" tmp1022_1519)
                      ", ")
                    tmp1024_1521)
                  ")"))
              end))
          end));
  in
    (
      case tmp1020_1515 of 
        Nil_517 => (tmp1021_1282 here k1514)
        | Cons_518 (tmp1026_1029, tmp1027_1028) => (tmp1025_1283
          here
          tmp1026_1029
          tmp1027_1028
          k1514)
      )
  end;


fun charAt_701 ev1286_1286 str_700 index_699 k1522 =
  (if (infixOr_131
    (infixLt_93 index_699 0)
    (infixLte_96 (length_703 str_700) index_699)) then (k1522
    (Some_379 (unsafeCharAt_722 str_700 index_699))) else (k1522 None_378));


fun substring_712 ev1287_1287 str_710 from_711 k1523 =
  (if (infixOr_131
    (infixLt_93 from_711 0)
    (infixLte_96 (length_703 str_710) from_711)) then (k1523 str_710) else (k1523
    (unsafeSubstring_715 str_710 from_711)));


fun toInt_719 ev1288_1288 str_718 k1524 =
  (toOption_443 ev1288_1288 (internalStringToInt_717 str_718) k1524);


fun safe_741 ev1289_1289 queen_738 diag_739 solution_740 k1525 =
  let
    val tmp800_1526 = solution_740;
    fun tmp801_1290 ev1292_1292 k1527 = (k1527 true);
    fun tmp803_1291 ev1293_1293 q_751 qs_752 k1528 =
      let val tmp802_1529 =
        (safe_741
          (nested ev1293_1293 ev1289_1289)
          queen_738
          (infixAdd_31 diag_739 1)
          qs_752
          (fn a1530 =>
            a1530));
      in (k1528
        (infixAnd_134
          (infixAnd_134
            (infixAnd_134
              (infixNeq_90 queen_738 q_751)
              (infixNeq_90 queen_738 (infixAdd_31 q_751 diag_739)))
            (infixNeq_90 queen_738 (infixSub_40 q_751 diag_739)))
          tmp802_1529))
      end;
  in
    (
      case tmp800_1526 of 
        Nil_517 => (tmp801_1290 here k1525)
        | Cons_518 (tmp804_807, tmp805_806) => (tmp803_1291
          here
          tmp804_807
          tmp805_806
          k1525)
      )
  end;


fun find_solutions_744 ev1294_1294 ev1295_1295 ev1297_1297 n_742 col_743 FailDollarcapability_1296 ChooseDollarcapability_1298 k1531 =
  (if (infixEq_86 col_743 0) then (k1531 Nil_517) else (find_solutions_744
    ev1294_1294
    ev1295_1295
    ev1297_1297
    n_742
    (infixSub_40 col_743 1)
    FailDollarcapability_1296
    ChooseDollarcapability_1298
    (fn a1532 =>
      let val sol_753 = a1532;
      in ((Choose_749 ChooseDollarcapability_1298)
        ev1297_1297
        n_742
        (fn a1533 =>
          let
            val queen_754 = a1533;
            val tmp815_1534 =
              (safe_741 ev1294_1294 queen_754 1 sol_753 (fn a1535 => a1535));
          in
            (if tmp815_1534 then (k1531 (Cons_518 (queen_754, sol_753))) else ((Fail_750
                FailDollarcapability_1296)
              ev1295_1295
              (fn a1536 =>
                let val f__1537 = a1536;
                in (k1531 Nil_517)
                end)))
          end))
      end)));


fun count_solutions_746 ev1299_1299 n_745 k1538 =
  ((withRegion
      (fn ev1300_1300 => fn this_840 => fn k1539 =>
        ((((fn ev1305_1305 => fn FailDollarcapability_1306 => fn ChooseDollarcapability_1307 => fn k1567 =>
                (find_solutions_744
                  (nested (nested ev1305_1305 ev1300_1300) ev1299_1299)
                  here
                  here
                  n_745
                  n_745
                  FailDollarcapability_1306
                  ChooseDollarcapability_1307
                  (fn a1568 =>
                    let val f__1569 = a1568;
                    in (k1567 1)
                    end)))
              lift
              (Fail_736 (fn ev1301_1301 => fn k1540 =>
                ((ev1301_1301
                    (fn k1541 =>
                      let fun resume_755 ev1543 a1542 =
                        (ev1543 (k1541 a1542));
                      in (fn k1544 => (k1544 0))
                      end))
                  k1540)))
              (Choose_737 (fn ev1302_1302 => fn n_756 => fn k1545 =>
                ((ev1302_1302
                    (fn k1546 =>
                      let fun resume_757 ev1548 a1547 =
                        (ev1548 (k1546 a1547));
                      in (fn k1549 =>
                        let
                          val tmp824_1550 = 1;
                          val i_758 = (fresh this_840 tmp824_1550);
                          val tmp825_1551 = 0;
                          val acc_759 = (fresh this_840 tmp825_1551);
                          fun tmp826_1303 ev1304_1304 k1552 =
                            let val tmp827_1553 = (!i_758);
                            in (if (infixNeq_90 tmp827_1553 n_756) then let
                              val tmp829_1554 = (!acc_759);
                              val tmp830_1555 = (!i_758);
                              val tmp831_1556 =
                                (resume_757
                                  here
                                  tmp830_1555
                                  (fn a1557 =>
                                    a1557));
                              val tmp832_1558 =
                                (acc_759 := (infixAdd_31
                                  tmp829_1554
                                  tmp831_1556));
                              val _ = tmp832_1558;
                              val tmp833_1559 = (!i_758);
                              val tmp828_1560 =
                                (i_758 := (infixAdd_31 tmp833_1559 1));
                            in (tmp826_1303 ev1304_1304 k1552)
                            end else (k1552 ()))
                            end;
                        in
                          (tmp826_1303
                            here
                            (fn a1561 =>
                              let
                                val tmp836_1562 = a1561;
                                val _ = tmp836_1562;
                                val tmp837_1563 = (!i_758);
                                val tmp838_1564 =
                                  (resume_757
                                    here
                                    tmp837_1563
                                    (fn a1565 =>
                                      a1565));
                                val tmp839_1566 = (!acc_759);
                              in
                                (k1549
                                  (infixAdd_31 tmp838_1564 tmp839_1566))
                              end))
                        end)
                      end))
                  k1545))))
            (fn a1570 =>
              (fn k21571 =>
                (k21571 a1570))))
          k1539)))
    k1538);


fun main_747 ev1308_1308 k1572 =
  let
    val tmp845_1573 =
      (count_solutions_746 ev1308_1308 8 (fn a1574 => a1574));
    val tmp846_1575 = (println_5 tmp845_1573);
  in (k1572 tmp846_1575)
  end;

(main_747 (fn a => a) (fn a => a));
