unit SuperViewZoneConfig;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TSuperViewPath = string[6];
  //TSuperViewPath = WideString;
  TSuperViewInfo = variant;

const
  cSuperViewLogCount = 1000;
  // must be smallest that cSuperViewLogCount
  cSuperViewLogCountWithObj = cSuperViewLogCount;

implementation

end.

