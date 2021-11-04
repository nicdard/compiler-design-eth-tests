%fty = type i64 (i64, i64, i64, i64, i64, i64, i64, i64, i64, i64)

declare i64 @ll_callback(%fty*)

define i64 @foo(i64 %x1, i64 %x2, i64 %x3, i64 %x4, i64 %x5, i64 %x6, i64 %x7, i64 %x8, i64 %x9, i64 %x10) {
    %b1 = icmp eq i64 %x1, 1
    %b2 = icmp eq i64 %x2, 2
    %b3 = icmp eq i64 %x3, 3
    %b4 = icmp eq i64 %x4, 4
    %b5 = icmp eq i64 %x5, 5
    %b6 = icmp eq i64 %x6, 6
    %b7 = icmp eq i64 %x7, 7
    %b8 = icmp eq i64 %x8, 8
    %b9 = icmp eq i64 %x9, 9
    %b10 = icmp eq i64 %x10, 10
    %a1 = and i64 %b1, %b2
    %a2 = and i64 %a1, %b3
    %a3 = and i64 %a2, %b4
    %a4 = and i64 %a3, %b5
    %a5 = and i64 %a4, %b6
    %a6 = and i64 %a5, %b7
    %a7 = and i64 %a6, %b8
    %a8 = and i64 %a7, %b9
    %a9 = and i64 %a8, %b10
    ret i64 %a9
}

define i64 @main(i64 %argc, i8** %argv) {
  %1 = call i64 @ll_callback(%fty* @foo)
  ret i64 %1
}