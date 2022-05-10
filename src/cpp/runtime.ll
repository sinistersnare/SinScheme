; ModuleID = 'runtime.cpp'
source_filename = "runtime.cpp"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

%struct.SinRecord = type { %struct.SinObj**, %struct.SinRecord*, i64, i64 }
%struct.SinObj = type { i8*, i32 }

@.str = private unnamed_addr constant [4 x i8] c"%p\0A\00", align 1

; Function Attrs: noinline nounwind optnone uwtable
define dso_local %struct.SinRecord* @make_frame(%struct.SinRecord* %0) #0 {
  %2 = alloca %struct.SinRecord*, align 8
  %3 = alloca %struct.SinRecord*, align 8
  %4 = alloca %struct.SinObj**, align 8
  store %struct.SinRecord* %0, %struct.SinRecord** %2, align 8
  %5 = call noalias i8* @malloc(i64 32) #4
  %6 = bitcast i8* %5 to %struct.SinRecord*
  store %struct.SinRecord* %6, %struct.SinRecord** %3, align 8
  %7 = call noalias i8* @calloc(i64 4096, i64 8) #4
  %8 = bitcast i8* %7 to %struct.SinObj**
  store %struct.SinObj** %8, %struct.SinObj*** %4, align 8
  %9 = load %struct.SinObj**, %struct.SinObj*** %4, align 8
  %10 = load %struct.SinRecord*, %struct.SinRecord** %3, align 8
  %11 = getelementptr inbounds %struct.SinRecord, %struct.SinRecord* %10, i32 0, i32 0
  store %struct.SinObj** %9, %struct.SinObj*** %11, align 8
  %12 = load %struct.SinRecord*, %struct.SinRecord** %3, align 8
  %13 = getelementptr inbounds %struct.SinRecord, %struct.SinRecord* %12, i32 0, i32 2
  store i64 512, i64* %13, align 8
  %14 = load %struct.SinRecord*, %struct.SinRecord** %2, align 8
  %15 = load %struct.SinRecord*, %struct.SinRecord** %3, align 8
  %16 = getelementptr inbounds %struct.SinRecord, %struct.SinRecord* %15, i32 0, i32 1
  store %struct.SinRecord* %14, %struct.SinRecord** %16, align 8
  %17 = load %struct.SinRecord*, %struct.SinRecord** %3, align 8
  ret %struct.SinRecord* %17
}

; Function Attrs: nounwind
declare dso_local noalias i8* @malloc(i64) #1

; Function Attrs: nounwind
declare dso_local noalias i8* @calloc(i64, i64) #1

; Function Attrs: noinline nounwind optnone uwtable
define dso_local %struct.SinRecord* @split_record(%struct.SinRecord* %0, %struct.SinObj** %1, i64 %2, i64 %3) #0 {
  %5 = alloca %struct.SinRecord*, align 8
  %6 = alloca %struct.SinObj**, align 8
  %7 = alloca i64, align 8
  %8 = alloca i64, align 8
  %9 = alloca %struct.SinRecord*, align 8
  store %struct.SinRecord* %0, %struct.SinRecord** %5, align 8
  store %struct.SinObj** %1, %struct.SinObj*** %6, align 8
  store i64 %2, i64* %7, align 8
  store i64 %3, i64* %8, align 8
  %10 = call noalias i8* @malloc(i64 32) #4
  %11 = bitcast i8* %10 to %struct.SinRecord*
  store %struct.SinRecord* %11, %struct.SinRecord** %9, align 8
  %12 = load %struct.SinObj**, %struct.SinObj*** %6, align 8
  %13 = load %struct.SinRecord*, %struct.SinRecord** %9, align 8
  %14 = getelementptr inbounds %struct.SinRecord, %struct.SinRecord* %13, i32 0, i32 0
  store %struct.SinObj** %12, %struct.SinObj*** %14, align 8
  %15 = load i64, i64* %7, align 8
  %16 = sub i64 512, %15
  %17 = load %struct.SinRecord*, %struct.SinRecord** %9, align 8
  %18 = getelementptr inbounds %struct.SinRecord, %struct.SinRecord* %17, i32 0, i32 2
  store i64 %16, i64* %18, align 8
  %19 = load %struct.SinRecord*, %struct.SinRecord** %5, align 8
  %20 = load %struct.SinRecord*, %struct.SinRecord** %9, align 8
  %21 = getelementptr inbounds %struct.SinRecord, %struct.SinRecord* %20, i32 0, i32 1
  store %struct.SinRecord* %19, %struct.SinRecord** %21, align 8
  %22 = load i64, i64* %7, align 8
  %23 = load %struct.SinRecord*, %struct.SinRecord** %5, align 8
  %24 = getelementptr inbounds %struct.SinRecord, %struct.SinRecord* %23, i32 0, i32 2
  store i64 %22, i64* %24, align 8
  %25 = load i64, i64* %8, align 8
  %26 = load %struct.SinRecord*, %struct.SinRecord** %5, align 8
  %27 = getelementptr inbounds %struct.SinRecord, %struct.SinRecord* %26, i32 0, i32 3
  store i64 %25, i64* %27, align 8
  %28 = load %struct.SinRecord*, %struct.SinRecord** %9, align 8
  ret %struct.SinRecord* %28
}

; Function Attrs: noinline norecurse optnone uwtable
define dso_local i32 @main() #2 {
  %1 = alloca i32, align 4
  %2 = alloca %struct.SinRecord*, align 8
  %3 = alloca %struct.SinObj**, align 8
  %4 = alloca %struct.SinRecord*, align 8
  store i32 0, i32* %1, align 4
  %5 = call %struct.SinRecord* @make_frame(%struct.SinRecord* null)
  store %struct.SinRecord* %5, %struct.SinRecord** %2, align 8
  %6 = load %struct.SinRecord*, %struct.SinRecord** %2, align 8
  %7 = getelementptr inbounds %struct.SinRecord, %struct.SinRecord* %6, i32 0, i32 0
  %8 = load %struct.SinObj**, %struct.SinObj*** %7, align 8
  store %struct.SinObj** %8, %struct.SinObj*** %3, align 8
  %9 = load %struct.SinRecord*, %struct.SinRecord** %2, align 8
  %10 = getelementptr inbounds %struct.SinRecord, %struct.SinRecord* %9, i32 0, i32 1
  %11 = load %struct.SinRecord*, %struct.SinRecord** %10, align 8
  store %struct.SinRecord* %11, %struct.SinRecord** %4, align 8
  %12 = load %struct.SinObj**, %struct.SinObj*** %3, align 8
  %13 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @.str, i64 0, i64 0), %struct.SinObj** %12)
  ret i32 0
}

declare dso_local i32 @printf(i8*, ...) #3

attributes #0 = { noinline nounwind optnone uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "frame-pointer"="all" "less-precise-fpmad"="false" "min-legal-vector-width"="0" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { nounwind "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "frame-pointer"="all" "less-precise-fpmad"="false" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #2 = { noinline norecurse optnone uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "frame-pointer"="all" "less-precise-fpmad"="false" "min-legal-vector-width"="0" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #3 = { "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "frame-pointer"="all" "less-precise-fpmad"="false" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #4 = { nounwind }

!llvm.module.flags = !{!0}
!llvm.ident = !{!1}

!0 = !{i32 1, !"wchar_size", i32 4}
!1 = !{!"clang version 10.0.0-4ubuntu1 "}
