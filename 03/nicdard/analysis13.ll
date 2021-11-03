define i64 @main(i64 %argc, i8** %arcv) {
  %1 = add i64 0, 0
  %2 = add i64 0, 1
  %3 = add i64 0, 2
  %4 = add i64 0, 3
  %5 = add i64 0, 4
  %6 = add i64 0, 5
  %7 = add i64 0, 7
  %cmp1 = icmp sgt i64 3, 0
  br i1 %cmp1, label %one, label %wrong
one:
  %cmp2 = icmp eq i64 1, %2
  br i1 %cmp2, label %two, label %wrong
two:
  %cmp3 = icmp ne i64 %3, 3
  br i1 %cmp3, label %three, label %wrong
three:
  %cmp4 = icmp slt i64 %4, 4
  br i1 %cmp4, label %four, label %wrong
four:
  %cmp5 = icmp sle i64 %5, 10
  br i1 %cmp5, label %five, label %wrong
five:
  %cmp6 = icmp sge i64 10, %6
  br i1 %cmp6, label %correct, label %wrong
correct:
  ret i64 %7
wrong:
  ret i64 %1
}

