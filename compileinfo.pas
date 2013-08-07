Unit CompileInfo;
// PlusAlpha Project
// Return Compiler Related Info for bug tracking purposes
// 2013.08.07 Created
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

Const
  CompileDate = {$I %DATE%};
  TargetArch = {$I %FPCTARGETCPU%};
  // Platform that the compiler runs on
  CompilerVersion = {$I %FPCVERSION%};
  {$IFDEF Darwin}
    CompilePlat = 'Darwin';
  {$ELSE}
    {$IFDEF WINDOWS}
      CompilePlat = 'Windows';
    {$ELSE}
      {$IFDEF UNIX}
        CompilePlat = 'Unix';
      {$ELSE}
        CompilePlat = 'Unknown Platform';
      {$ENDIF}
    {$ENDIF}
  {$ENDIF}
  // Platform that the compiler is targeting (platform the program runs on).
  TargetPlat = {$I %FPCTARGETOS%};

Function AllCompileInfo:UTF8String;


implementation

Function AllCompileInfo:UTF8String;
 begin
   Result := 'Compiled: ' + CompileDate + '/' + TargetArch+'/'+TargetPlat+'/'+CompilerVersion;
 end;

end.

