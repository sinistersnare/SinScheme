; ModuleID = 'runtime.cpp'
source_filename = "runtime.cpp"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

%struct.SinFrame = type { i64*, %struct.SinFrame*, i64, i8* }

; Function Attrs: noinline nounwind optnone uwtable
define dso_local %struct.SinFrame* @_Z10make_frameP8SinFramePx(%struct.SinFrame* %0, i64* %1) #0 {
  %3 = alloca %struct.SinFrame*, align 8
  %4 = alloca i64*, align 8
  %5 = alloca %struct.SinFrame*, align 8
  %6 = alloca i64*, align 8
  store %struct.SinFrame* %0, %struct.SinFrame** %3, align 8
  store i64* %1, i64** %4, align 8
  %7 = call noalias i8* @malloc(i64 32) #3
  %8 = bitcast i8* %7 to %struct.SinFrame*
  store %struct.SinFrame* %8, %struct.SinFrame** %5, align 8
  %9 = call noalias i8* @calloc(i64 4096, i64 8) #3
  %10 = bitcast i8* %9 to i64*
  store i64* %10, i64** %6, align 8
  %11 = load i64*, i64** %6, align 8
  %12 = load %struct.SinFrame*, %struct.SinFrame** %5, align 8
  %13 = getelementptr inbounds %struct.SinFrame, %struct.SinFrame* %12, i32 0, i32 0
  store i64* %11, i64** %13, align 8
  %14 = load %struct.SinFrame*, %struct.SinFrame** %5, align 8
  %15 = getelementptr inbounds %struct.SinFrame, %struct.SinFrame* %14, i32 0, i32 2
  store i64 512, i64* %15, align 8
  %16 = load %struct.SinFrame*, %struct.SinFrame** %3, align 8
  %17 = load %struct.SinFrame*, %struct.SinFrame** %5, align 8
  %18 = getelementptr inbounds %struct.SinFrame, %struct.SinFrame* %17, i32 0, i32 1
  store %struct.SinFrame* %16, %struct.SinFrame** %18, align 8
  %19 = load i64*, i64** %4, align 8
  %20 = bitcast i64* %19 to i8*
  %21 = load %struct.SinFrame*, %struct.SinFrame** %5, align 8
  %22 = getelementptr inbounds %struct.SinFrame, %struct.SinFrame* %21, i32 0, i32 3
  store i8* %20, i8** %22, align 8
  %23 = load %struct.SinFrame*, %struct.SinFrame** %5, align 8
  ret %struct.SinFrame* %23
}

; Function Attrs: nounwind
declare dso_local noalias i8* @malloc(i64) #1

; Function Attrs: nounwind
declare dso_local noalias i8* @calloc(i64, i64) #1

; Function Attrs: noinline norecurse nounwind optnone uwtable
define dso_local i32 @main() #2 {
  %1 = alloca i32, align 4
  %2 = alloca i64*, align 8
  %3 = alloca i64*, align 8
  %4 = alloca i64*, align 8
  store i32 0, i32* %1, align 4
  %5 = call noalias i8* @calloc(i64 4096, i64 8) #3
  %6 = bitcast i8* %5 to i64*
  store i64* %6, i64** %2, align 8
  %7 = load i64*, i64** %2, align 8
  %8 = getelementptr inbounds i64, i64* %7, i64 10
  store i64* %8, i64** %3, align 8
  %9 = load i64*, i64** %3, align 8
  %10 = getelementptr inbounds i64, i64* %9, i64 -3
  store i64* %10, i64** %4, align 8
  ret i32 2
}

attributes #0 = { noinline nounwind optnone uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "frame-pointer"="all" "less-precise-fpmad"="false" "min-legal-vector-width"="0" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { nounwind "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "frame-pointer"="all" "less-precise-fpmad"="false" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #2 = { noinline norecurse nounwind optnone uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "frame-pointer"="all" "less-precise-fpmad"="false" "min-legal-vector-width"="0" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #3 = { nounwind }

!llvm.module.flags = !{!0}
!llvm.ident = !{!1}

!0 = !{i32 1, !"wchar_size", i32 4}
!1 = !{!"clang version 10.0.0-4ubuntu1 "}
