datatype ('arg1302, 'arg1303) Tuple2_137 =
  Tuple2_197 of ('arg1302 * 'arg1303);


fun first_200 (Tuple2_197 (first_200, _)) = first_200;


fun second_201 (Tuple2_197 (_, second_201)) = second_201;


datatype ('arg1304, 'arg1305, 'arg1306) Tuple3_141 =
  Tuple3_202 of ('arg1304 * 'arg1305 * 'arg1306);


fun firstDollar1_206 (Tuple3_202 (firstDollar1_206, _, _)) =
  firstDollar1_206;


fun secondDollar1_207 (Tuple3_202 (_, secondDollar1_207, _)) =
  secondDollar1_207;


fun third_208 (Tuple3_202 (_, _, third_208)) = third_208;


datatype ('arg1307, 'arg1308, 'arg1309, 'arg1310) Tuple4_146 =
  Tuple4_209 of ('arg1307 * 'arg1308 * 'arg1309 * 'arg1310);


fun firstDollar2_214 (Tuple4_209 (firstDollar2_214, _, _, _)) =
  firstDollar2_214;


fun secondDollar2_215 (Tuple4_209 (_, secondDollar2_215, _, _)) =
  secondDollar2_215;


fun thirdDollar1_216 (Tuple4_209 (_, _, thirdDollar1_216, _)) =
  thirdDollar1_216;


fun fourth_217 (Tuple4_209 (_, _, _, fourth_217)) = fourth_217;


datatype ('arg1311, 'arg1312, 'arg1313, 'arg1314, 'arg1315) Tuple5_152 =
  Tuple5_218 of ('arg1311 * 'arg1312 * 'arg1313 * 'arg1314 * 'arg1315);


fun firstDollar3_224 (Tuple5_218 (firstDollar3_224, _, _, _, _)) =
  firstDollar3_224;


fun secondDollar3_225 (Tuple5_218 (_, secondDollar3_225, _, _, _)) =
  secondDollar3_225;


fun thirdDollar2_226 (Tuple5_218 (_, _, thirdDollar2_226, _, _)) =
  thirdDollar2_226;


fun fourthDollar1_227 (Tuple5_218 (_, _, _, fourthDollar1_227, _)) =
  fourthDollar1_227;


fun fifth_228 (Tuple5_218 (_, _, _, _, fifth_228)) = fifth_228;


datatype ('arg1316, 'arg1317, 'arg1318, 'arg1319, 'arg1320, 'arg1321) Tuple6_159 =
  Tuple6_229 of ('arg1316 * 'arg1317 * 'arg1318 * 'arg1319 * 'arg1320 * 'arg1321);


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


datatype 'arg1322 Exception_164 =
  Exception_164 of 'arg1322;


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


datatype 'arg1323 Set_737 =
  Set_737 of 'arg1323;


fun Set_745 (Set_737 Set_745) = Set_745;


datatype 'arg1324 Get_736 =
  Get_736 of 'arg1324;


fun Get_746 (Get_736 Get_746) = Get_746;


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


fun locally_3 ev1125_1125 ev1126_1126 f_1 k1325 = (f_1 ev1126_1126 k1325);


val PI_196 = (f_pi_74 ());


fun raise_168 ev1127_1127 ev1128_1128 msg_167 ExceptionDollarcapability_1129 k1326 =
  ((raise_244 ExceptionDollarcapability_1129)
    ev1128_1128
    RuntimeError_245
    msg_167
    (fn a1329 =>
      let
        val tmp1075_1330 = a1329;
        val tmp1076_1331 = tmp1075_1330;
      in (raise Hole)
      end));


fun raiseDollar1_173 ev1131_1131 ev1132_1132 exception_171 msg_172 ExceptionDollarcapability_1133 k1332 =
  ((raise_244 ExceptionDollarcapability_1133)
    ev1132_1132
    exception_171
    msg_172
    (fn a1333 =>
      let
        val tmp1079_1334 = a1333;
        val tmp1080_1335 = tmp1079_1334;
      in (raise Hole)
      end));


fun panicOn_176 ev1134_1134 ev1135_1135 prog_175 k1336 =
  ((((fn ev1137_1137 => fn ExceptionDollarcapability_1138 => fn k1343 =>
          (prog_175
            (nested ev1137_1137 ev1135_1135)
            here
            ExceptionDollarcapability_1138
            k1343))
        lift
        (Exception_164 (fn ev1136_1136 => fn exception_246 => fn msg_247 => fn k1337 =>
          ((ev1136_1136
              (fn k1338 =>
                let fun resume_248 ev1340 a1339 = (ev1340 (k1338 a1339));
                in (fn k1341 =>
                  let val tmp1085_1342 = (panic_162 msg_247);
                  in (k1341 tmp1085_1342)
                  end)
                end))
            k1337))))
      (fn a1344 =>
        (fn k21345 =>
          (k21345 a1344))))
    k1336);


fun report_179 ev1139_1139 ev1140_1140 prog_178 k1346 =
  ((((fn ev1142_1142 => fn ExceptionDollarcapability_1143 => fn k1353 =>
          (prog_178
            (nested ev1142_1142 ev1140_1140)
            here
            ExceptionDollarcapability_1143
            k1353))
        lift
        (Exception_164 (fn ev1141_1141 => fn exception_249 => fn msg_250 => fn k1347 =>
          ((ev1141_1141
              (fn k1348 =>
                let fun resume_251 ev1350 a1349 = (ev1350 (k1348 a1349));
                in (fn k1351 =>
                  let val tmp1090_1352 = (printlnDollar3_11 msg_250);
                  in (k1351 tmp1090_1352)
                  end)
                end))
            k1347))))
      (fn a1354 =>
        (fn k21355 =>
          (k21355 a1354))))
    k1346);


fun ignoring_182 ev1144_1144 ev1145_1145 prog_181 k1356 =
  ((((fn ev1147_1147 => fn ExceptionDollarcapability_1148 => fn k1362 =>
          (prog_181
            (nested ev1147_1147 ev1145_1145)
            here
            ExceptionDollarcapability_1148
            k1362))
        lift
        (Exception_164 (fn ev1146_1146 => fn exception_252 => fn msg_253 => fn k1357 =>
          ((ev1146_1146
              (fn k1358 =>
                let fun resume_254 ev1360 a1359 = (ev1360 (k1358 a1359));
                in (fn k1361 => (k1361 ()))
                end))
            k1357))))
      (fn a1363 =>
        (fn k21364 =>
          (k21364 a1363))))
    k1356);


fun each_186 ev1149_1149 ev1150_1150 start_183 end_184 action_185 k1365 =
  let fun loop_256 ev1151_1151 i_255 k1366 =
    (if (infixLt_93 i_255 end_184) then (action_185
      (nested ev1151_1151 ev1150_1150)
      i_255
      (fn a1367 =>
        let val f__1368 = a1367;
        in (loop_256 ev1151_1151 (infixAdd_31 i_255 1) k1366)
        end)) else (k1366 ()));
  in (loop_256 here start_183 k1365)
  end;


fun repeat_189 ev1152_1152 ev1153_1153 n_187 action_188 k1369 =
  (each_186
    ev1152_1152
    here
    0
    n_187
    (fn ev1154_1154 => fn n_257 => fn k1370 =>
      (action_188 (nested ev1154_1154 ev1153_1153) k1370))
    k1369);


fun timed_191 ev1155_1155 ev1156_1156 block_190 k1371 =
  let val before_258 =
    let val tmp1109_1372 = (timestamp_15 ());
    in tmp1109_1372
    end;
  in (block_190
    ev1156_1156
    (fn a1373 =>
      let
        val f__1374 = a1373;
        val after_259 =
          let val tmp1112_1375 = (timestamp_15 ());
          in tmp1112_1375
          end;
      in (k1371 (infixMul_34 1000 (infixSub_40 after_259 before_258)))
      end))
  end;


fun measure_195 ev1157_1157 ev1158_1158 warmup_192 iterations_193 block_194 k1376 =
  let
    fun run_262 ev1159_1159 n_260 report_261 k1377 =
      (if (infixLte_96 n_260 0) then (k1377 ()) else (timed_191
        (nested ev1159_1159 ev1157_1157)
        here
        (fn ev1160_1160 => fn k1378 =>
          (block_194
            (nested (nested ev1160_1160 ev1159_1159) ev1158_1158)
            k1378))
        (fn a1379 =>
          let
            val time_263 = a1379;
            val k1380 =
              (fn a1381 =>
                let
                  val tmp1118_1382 = a1381;
                  val _ = tmp1118_1382;
                in
                  (run_262
                    ev1159_1159
                    (infixSub_40 n_260 1)
                    report_261
                    k1377)
                end);
          in
            (if report_261 then let val tmp1117_1383 =
              (println_5 time_263);
            in (k1380 tmp1117_1383)
            end else (k1380 ()))
          end)));
    val _ = (run_262 here warmup_192 false (fn a1384 => a1384));
  in (run_262 here iterations_193 true k1376)
  end;


fun isDefined_353 ev1161_1161 self_352 k1385 =
  let
    val tmp1028_1386 = self_352;
    fun tmp1029_1162 ev1164_1164 k1387 = (k1387 false);
    fun tmp1030_1163 ev1165_1165 v_382 k1388 = (k1388 true);
  in
    (
      case tmp1028_1386 of 
        None_378 => (tmp1029_1162 here k1385)
        | Some_379 tmp1031_1389 => (tmp1030_1163 here tmp1031_1389 k1385)
      )
  end;


fun isEmpty_356 ev1166_1166 self_355 k1390 =
  let val tmp1034_1391 =
    (isDefined_353 ev1166_1166 self_355 (fn a1392 => a1392));
  in (k1390 (not_128 tmp1034_1391))
  end;


fun orElse_360 ev1167_1167 ev1168_1168 self_358 that_359 k1393 =
  let
    val tmp1035_1394 = self_358;
    fun tmp1038_1169 ev1171_1171 k1395 =
      (that_359 (nested ev1171_1171 ev1168_1168) k1395);
    fun tmp1039_1170 ev1172_1172 v_383 k1396 = (k1396 (Some_379 v_383));
  in
    (
      case tmp1035_1394 of 
        None_378 => (tmp1038_1169 here k1393)
        | Some_379 tmp1040_1397 => (tmp1039_1170 here tmp1040_1397 k1393)
      )
  end;


fun getOrElse_364 ev1173_1173 ev1174_1174 self_362 that_363 k1398 =
  let
    val tmp1043_1399 = self_362;
    fun tmp1046_1175 ev1177_1177 k1400 =
      (that_363 (nested ev1177_1177 ev1174_1174) k1400);
    fun tmp1047_1176 ev1178_1178 v_384 k1401 = (k1401 v_384);
  in
    (
      case tmp1043_1399 of 
        None_378 => (tmp1046_1175 here k1398)
        | Some_379 tmp1048_1402 => (tmp1047_1176 here tmp1048_1402 k1398)
      )
  end;


fun map_369 ev1179_1179 ev1180_1180 self_367 f_368 k1403 =
  let
    val tmp1051_1404 = self_367;
    fun tmp1052_1181 ev1183_1183 k1405 = (k1405 None_378);
    fun tmp1054_1182 ev1184_1184 v_385 k1406 =
      (f_368
        (nested ev1184_1184 ev1180_1180)
        v_385
        (fn a1407 =>
          let val tmp1053_1408 = a1407;
          in (k1406 (Some_379 tmp1053_1408))
          end));
  in
    (
      case tmp1051_1404 of 
        None_378 => (tmp1052_1181 here k1403)
        | Some_379 tmp1055_1409 => (tmp1054_1182 here tmp1055_1409 k1403)
      )
  end;


fun foreach_373 ev1185_1185 ev1186_1186 self_371 f_372 k1410 =
  let
    val tmp1058_1411 = self_371;
    fun tmp1059_1187 ev1189_1189 k1412 = (k1412 ());
    fun tmp1062_1188 ev1190_1190 v_386 k1413 =
      (f_372 (nested ev1190_1190 ev1186_1186) v_386 k1413);
  in
    (
      case tmp1058_1411 of 
        None_378 => (tmp1059_1187 here k1410)
        | Some_379 tmp1063_1414 => (tmp1062_1188 here tmp1063_1414 k1410)
      )
  end;


fun show_377 ev1191_1191 ev1192_1192 o_375 showA_376 k1415 =
  let
    val tmp1066_1416 = o_375;
    fun tmp1067_1193 ev1195_1195 k1417 = (k1417 "None()");
    fun tmp1069_1194 ev1196_1196 v_387 k1418 =
      (showA_376
        (nested ev1196_1196 ev1192_1192)
        v_387
        (fn a1419 =>
          let val tmp1068_1420 = a1419;
          in (k1418
            (infixConcat_28 (infixConcat_28 "Some(" tmp1068_1420) ")"))
          end));
  in
    (
      case tmp1066_1416 of 
        None_378 => (tmp1067_1193 here k1415)
        | Some_379 tmp1070_1421 => (tmp1069_1194 here tmp1070_1421 k1415)
      )
  end;


fun isEmpty_437 ev1197_1197 opt_436 k1422 =
  (k1422 (not_128 (isDefined_434 opt_436)));


fun toOption_443 ev1198_1198 opt_442 k1423 =
  (if (isDefined_434 opt_442) then (k1423 (Some_379 (force_440 opt_442))) else (k1423
    None_378));


fun isEmpty_464 ev1199_1199 l_463 k1424 =
  let
    val tmp847_1425 = l_463;
    fun tmp848_1200 ev1202_1202 k1426 = (k1426 true);
    fun tmp849_1201 ev1203_1203 a_523 rest_524 k1427 = (k1427 false);
  in
    (
      case tmp847_1425 of 
        Nil_517 => (tmp848_1200 here k1424)
        | Cons_518 (tmp850_853, tmp851_852) => (tmp849_1201
          here
          tmp850_853
          tmp851_852
          k1424)
      )
  end;


fun foreach_468 ev1204_1204 ev1205_1205 l_466 f_467 k1428 =
  let fun loop_526 ev1206_1206 remainder_525 k1429 =
    let val tmp856_1430 =
      (isEmpty_464
        (nested ev1206_1206 ev1204_1204)
        remainder_525
        (fn a1431 =>
          a1431));
    in (if (not_128 tmp856_1430) then let
      val tmp857_1432 = remainder_525;
      fun tmp858_1207 ev1209_1209 k1433 = (k1433 ());
      fun tmp863_1208 ev1210_1210 a_527 as_528 k1434 =
        (f_467
          (nested (nested ev1210_1210 ev1206_1206) ev1205_1205)
          a_527
          (fn a1435 =>
            let val f__1436 = a1435;
            in (loop_526 (nested ev1210_1210 ev1206_1206) as_528 k1434)
            end));
    in
      (
        case tmp857_1432 of 
          Nil_517 => (tmp858_1207 here k1429)
          | Cons_518 (tmp864_867, tmp865_866) => (tmp863_1208
            here
            tmp864_867
            tmp865_866
            k1429)
        )
    end else (k1429 ()))
    end;
  in (loop_526 here l_466 k1428)
  end;


fun size_471 ev1211_1211 l_470 k1437 =
  let fun loop_531 ev1212_1212 lst_529 acc_530 k1438 =
    let
      val tmp874_1439 = lst_529;
      fun tmp875_1213 ev1215_1215 k1440 = (k1440 acc_530);
      fun tmp878_1214 ev1216_1216 a_532 as_533 k1441 =
        (loop_531
          (nested ev1216_1216 ev1212_1212)
          as_533
          (infixAdd_31 acc_530 1)
          k1441);
    in
      (
        case tmp874_1439 of 
          Nil_517 => (tmp875_1213 here k1438)
          | Cons_518 (tmp879_882, tmp880_881) => (tmp878_1214
            here
            tmp879_882
            tmp880_881
            k1438)
        )
    end;
  in (loop_531 here l_470 0 k1437)
  end;


fun reverse_474 ev1217_1217 l_473 k1442 =
  let fun loop_536 ev1218_1218 lst_534 acc_535 k1443 =
    let
      val tmp887_1444 = lst_534;
      fun tmp888_1219 ev1221_1221 k1445 = (k1445 acc_535);
      fun tmp891_1220 ev1222_1222 a_537 as_538 k1446 =
        (loop_536
          (nested ev1222_1222 ev1218_1218)
          as_538
          (Cons_518 (a_537, acc_535))
          k1446);
    in
      (
        case tmp887_1444 of 
          Nil_517 => (tmp888_1219 here k1443)
          | Cons_518 (tmp892_895, tmp893_894) => (tmp891_1220
            here
            tmp892_895
            tmp893_894
            k1443)
        )
    end;
  in (loop_536 here l_473 Nil_517 k1442)
  end;


fun map_479 ev1223_1223 ev1224_1224 l_477 f_478 k1447 =
  let fun loop_541 ev1225_1225 lst_539 acc_540 k1448 =
    let
      val tmp900_1449 = lst_539;
      fun tmp901_1226 ev1228_1228 k1450 = (k1450 acc_540);
      fun tmp905_1227 ev1229_1229 a_542 as_543 k1451 =
        (f_478
          (nested (nested ev1229_1229 ev1225_1225) ev1224_1224)
          a_542
          (fn a1452 =>
            let val tmp902_1453 = a1452;
            in (loop_541
              (nested ev1229_1229 ev1225_1225)
              as_543
              (Cons_518 (tmp902_1453, acc_540))
              k1451)
            end));
    in
      (
        case tmp900_1449 of 
          Nil_517 => (tmp901_1226 here k1448)
          | Cons_518 (tmp906_909, tmp907_908) => (tmp905_1227
            here
            tmp906_909
            tmp907_908
            k1448)
        )
    end;
  in (loop_541
    here
    l_477
    Nil_517
    (fn a1454 =>
      let val tmp912_1455 = a1454;
      in (reverse_474 ev1223_1223 tmp912_1455 k1447)
      end))
  end;


fun reverseOnto_483 ev1230_1230 l_481 other_482 k1456 =
  let
    val tmp915_1457 = l_481;
    fun tmp916_1231 ev1233_1233 k1458 = (k1458 other_482);
    fun tmp919_1232 ev1234_1234 a_544 rest_545 k1459 =
      (reverseOnto_483
        (nested ev1234_1234 ev1230_1230)
        rest_545
        (Cons_518 (a_544, other_482))
        k1459);
  in
    (
      case tmp915_1457 of 
        Nil_517 => (tmp916_1231 here k1456)
        | Cons_518 (tmp920_923, tmp921_922) => (tmp919_1232
          here
          tmp920_923
          tmp921_922
          k1456)
      )
  end;


fun append_487 ev1235_1235 l_485 other_486 k1460 =
  let val tmp926_1461 =
    (reverse_474 ev1235_1235 l_485 (fn a1462 => a1462));
  in (reverseOnto_483 ev1235_1235 tmp926_1461 other_486 k1460)
  end;


fun take_491 ev1236_1236 l_489 n_490 k1463 =
  (if (infixEq_86 n_490 0) then (k1463 Nil_517) else let
    val tmp929_1464 = l_489;
    fun tmp930_1237 ev1239_1239 k1465 = (k1465 Nil_517);
    fun tmp932_1238 ev1240_1240 a_546 rest_547 k1466 =
      let val tmp931_1467 =
        (take_491
          (nested ev1240_1240 ev1236_1236)
          rest_547
          (infixSub_40 n_490 1)
          (fn a1468 =>
            a1468));
      in (k1466 (Cons_518 (a_546, tmp931_1467)))
      end;
  in
    (
      case tmp929_1464 of 
        Nil_517 => (tmp930_1237 here k1463)
        | Cons_518 (tmp933_936, tmp934_935) => (tmp932_1238
          here
          tmp933_936
          tmp934_935
          k1463)
      )
  end);


fun drop_495 ev1241_1241 l_493 n_494 k1469 =
  (if (infixEq_86 n_494 0) then (k1469 l_493) else let
    val tmp941_1470 = l_493;
    fun tmp942_1242 ev1244_1244 k1471 = (k1471 Nil_517);
    fun tmp945_1243 ev1245_1245 a_548 rest_549 k1472 =
      (drop_495
        (nested ev1245_1245 ev1241_1241)
        rest_549
        (infixSub_40 n_494 1)
        k1472);
  in
    (
      case tmp941_1470 of 
        Nil_517 => (tmp942_1242 here k1469)
        | Cons_518 (tmp946_949, tmp947_948) => (tmp945_1243
          here
          tmp946_949
          tmp947_948
          k1469)
      )
  end);


fun nonEmpty_498 ev1246_1246 l_497 k1473 =
  let
    val tmp954_1474 = l_497;
    fun tmp955_1247 ev1249_1249 k1475 = (k1475 false);
    fun tmp956_1248 ev1250_1250 a_550 rest_551 k1476 = (k1476 true);
  in
    (
      case tmp954_1474 of 
        Nil_517 => (tmp955_1247 here k1473)
        | Cons_518 (tmp957_960, tmp958_959) => (tmp956_1248
          here
          tmp957_960
          tmp958_959
          k1473)
      )
  end;


fun head_501 ev1251_1251 ev1252_1252 l_500 ExceptionDollarcapability_1253 k1477 =
  let
    val tmp963_1478 = l_500;
    fun tmp966_1254 ev1256_1256 k1479 =
      ((raise_244 ExceptionDollarcapability_1253)
        (nested ev1256_1256 ev1252_1252)
        EmptyList_516
        "Trying to get the head of an empty list"
        k1479);
    fun tmp967_1255 ev1257_1257 a_552 rest_553 k1480 = (k1480 a_552);
  in
    (
      case tmp963_1478 of 
        Nil_517 => (tmp966_1254 here k1477)
        | Cons_518 (tmp968_971, tmp969_970) => (tmp967_1255
          here
          tmp968_971
          tmp969_970
          k1477)
      )
  end;


fun tail_504 ev1258_1258 ev1259_1259 l_503 ExceptionDollarcapability_1260 k1481 =
  let
    val tmp974_1482 = l_503;
    fun tmp977_1261 ev1263_1263 k1483 =
      ((raise_244 ExceptionDollarcapability_1260)
        (nested ev1263_1263 ev1259_1259)
        EmptyList_516
        "Trying to get the head of an empty list"
        k1483);
    fun tmp978_1262 ev1264_1264 a_554 rest_555 k1484 = (k1484 rest_555);
  in
    (
      case tmp974_1482 of 
        Nil_517 => (tmp977_1261 here k1481)
        | Cons_518 (tmp979_982, tmp980_981) => (tmp978_1262
          here
          tmp979_982
          tmp980_981
          k1481)
      )
  end;


fun headOption_507 ev1265_1265 l_506 k1485 =
  let
    val tmp985_1486 = l_506;
    fun tmp986_1266 ev1268_1268 k1487 = (k1487 None_378);
    fun tmp987_1267 ev1269_1269 a_556 rest_557 k1488 =
      (k1488 (Some_379 a_556));
  in
    (
      case tmp985_1486 of 
        Nil_517 => (tmp986_1266 here k1485)
        | Cons_518 (tmp988_991, tmp989_990) => (tmp987_1267
          here
          tmp988_991
          tmp989_990
          k1485)
      )
  end;


fun partition_511 ev1270_1270 ev1271_1271 l_509 pred_510 k1489 =
  ((withRegion
      (fn ev1272_1272 => fn this_1011 => fn k1490 =>
        let
          val tmp994_1491 = Nil_517;
          val lefts_558 = (fresh this_1011 tmp994_1491);
          val tmp995_1493 = Nil_517;
          val rights_559 = (fresh this_1011 tmp995_1493);
        in
          (foreach_468
            (nested ev1272_1272 ev1270_1270)
            here
            l_509
            (fn ev1273_1273 => fn el_560 => fn k1494 =>
              (pred_510
                (nested (nested ev1273_1273 ev1272_1272) ev1271_1271)
                el_560
                (fn a1495 =>
                  let val tmp996_1496 = a1495;
                  in (if tmp996_1496 then let val tmp997_1497 =
                    (!lefts_558);
                  in (k1494 (lefts_558 := (Cons_518 (el_560, tmp997_1497))))
                  end else let val tmp1000_1498 = (!rights_559);
                  in (k1494
                    (rights_559 := (Cons_518 (el_560, tmp1000_1498))))
                  end)
                  end)))
            (fn a1499 =>
              let
                val f__1500 = a1499;
                val tmp1007_1501 = (!lefts_558);
                val tmp1008_1502 =
                  (reverse_474
                    (nested ev1272_1272 ev1270_1270)
                    tmp1007_1501
                    (fn a1503 =>
                      a1503));
                val tmp1009_1504 = (!rights_559);
                val tmp1010_1505 =
                  (reverse_474
                    (nested ev1272_1272 ev1270_1270)
                    tmp1009_1504
                    (fn a1506 =>
                      a1506));
              in (k1490 (Tuple2_197 (tmp1008_1502, tmp1010_1505)))
              end))
        end))
    k1489);


fun show_515 ev1274_1274 ev1275_1275 l_513 showA_514 k1507 =
  let
    val tmp1014_1508 = l_513;
    fun tmp1015_1276 ev1278_1278 k1509 = (k1509 "Nil()");
    fun tmp1019_1277 ev1279_1279 x_561 xs_562 k1510 =
      (showA_514
        (nested ev1279_1279 ev1275_1275)
        x_561
        (fn a1511 =>
          let val tmp1016_1512 = a1511;
          in (show_515
            (nested ev1279_1279 ev1274_1274)
            (nested ev1279_1279 ev1275_1275)
            xs_562
            showA_514
            (fn a1513 =>
              let val tmp1018_1514 = a1513;
              in (k1510
                (infixConcat_28
                  (infixConcat_28
                    (infixConcat_28
                      (infixConcat_28 "Cons(" tmp1016_1512)
                      ", ")
                    tmp1018_1514)
                  ")"))
              end))
          end));
  in
    (
      case tmp1014_1508 of 
        Nil_517 => (tmp1015_1276 here k1507)
        | Cons_518 (tmp1020_1023, tmp1021_1022) => (tmp1019_1277
          here
          tmp1020_1023
          tmp1021_1022
          k1507)
      )
  end;


fun charAt_701 ev1280_1280 str_700 index_699 k1515 =
  (if (infixOr_131
    (infixLt_93 index_699 0)
    (infixLte_96 (length_703 str_700) index_699)) then (k1515
    (Some_379 (unsafeCharAt_722 str_700 index_699))) else (k1515 None_378));


fun substring_712 ev1281_1281 str_710 from_711 k1516 =
  (if (infixOr_131
    (infixLt_93 from_711 0)
    (infixLte_96 (length_703 str_710) from_711)) then (k1516 str_710) else (k1516
    (unsafeSubstring_715 str_710 from_711)));


fun toInt_719 ev1282_1282 str_718 k1517 =
  (toOption_443 ev1282_1282 (internalStringToInt_717 str_718) k1517);


fun state_740 ev1283_1283 ev1284_1284 init_738 f_739 k1518 =
  ((withRegion
      (fn ev1285_1285 => fn this_826 => fn k1519 =>
        let
          val tmp814_1520 = init_738;
          val s_747 = (fresh this_826 tmp814_1520);
        in
          ((((fn ev1288_1288 => fn SetDollarcapability_1289 => fn GetDollarcapability_1290 => fn k1533 =>
                  (f_739
                    (nested (nested ev1288_1288 ev1285_1285) ev1284_1284)
                    here
                    here
                    GetDollarcapability_1290
                    SetDollarcapability_1289
                    k1533))
                lift
                (Set_737 (fn ev1286_1286 => fn n_748 => fn k1521 =>
                  ((ev1286_1286
                      (fn k1522 =>
                        let fun resume_749 ev1524 a1523 =
                          (ev1524 (k1522 a1523));
                        in (fn k1525 =>
                          let
                            val tmp818_1526 = (s_747 := n_748);
                            val _ = tmp818_1526;
                          in (resume_749 here () k1525)
                          end)
                        end))
                    k1521)))
                (Get_736 (fn ev1287_1287 => fn k1527 =>
                  ((ev1287_1287
                      (fn k1528 =>
                        let fun resume_750 ev1530 a1529 =
                          (ev1530 (k1528 a1529));
                        in (fn k1531 =>
                          let val tmp821_1532 = (!s_747);
                          in (resume_750 here tmp821_1532 k1531)
                          end)
                        end))
                    k1527))))
              (fn a1534 =>
                (fn k21535 =>
                  (k21535 a1534))))
            k1519)
        end))
    k1518);


fun counter_742 ev1291_1291 ev1292_1292 ev1294_1294 c_741 GetDollarcapability_1293 SetDollarcapability_1295 k1536 =
  ((Get_746 GetDollarcapability_1293)
    ev1292_1292
    (fn a1537 =>
      let val i_751 = a1537;
      in (if (infixEq_86 i_751 0) then (k1536 c_741) else ((Set_745
          SetDollarcapability_1295)
        ev1294_1294
        (infixSub_40 i_751 1)
        (fn a1538 =>
          let val f__1539 = a1538;
          in (counter_742
            ev1291_1291
            ev1292_1292
            ev1294_1294
            (infixAdd_31 c_741 1)
            GetDollarcapability_1293
            SetDollarcapability_1295
            k1536)
          end)))
      end));


fun main_743 ev1296_1296 k1540 =
  let
    val tmp839_1541 =
      (state_740
        ev1296_1296
        here
        100100100
        (fn ev1297_1297 => fn ev1298_1298 => fn ev1300_1300 => fn GetDollarcapability_1299 => fn SetDollarcapability_1301 => fn k1542 =>
          (counter_742
            (nested ev1297_1297 ev1296_1296)
            ev1298_1298
            ev1300_1300
            0
            GetDollarcapability_1299
            SetDollarcapability_1301
            k1542))
        (fn a1543 =>
          a1543));
    val tmp840_1544 = (println_5 tmp839_1541);
  in (k1540 tmp840_1544)
  end;

(main_743 (fn a => a) (fn a => a));
