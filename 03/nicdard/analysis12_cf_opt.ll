define i64 @main(i64 %argc, i8** %arcv) {
  %1 = add i64 5, 9
  %2 = sub i64 14, 15
  %3 = mul i64 -1, -1
  %4 = shl i64 1, 1
  %5 = lshr i64 2, 14
  %6 = ashr i64 0, 3
  %7 = and i64 -1, 14
  %8 = or i64 0, 14
  %9 = xor i64 14, 0
  ret i64 14
}

