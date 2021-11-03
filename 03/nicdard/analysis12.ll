define i64 @main(i64 %argc, i8** %arcv) {
  %1 = add i64 5, 9
  %2 = sub i64 %1, 15
  %3 = mul i64 %2, %2
  %4 = shl i64 %3, 1
  %5 = lshr i64 %4, %1
  %6 = ashr i64 %5, 3
  %7 = and i64 %2, %1
  %8 = or i64 %5, %7
  %9 = xor i64 %1, %5
  ret i64 %9
}

