
const
  CAppVersion = '0.1.2';
  CAppName = 'Velena GUI';
  CBuild = 'build ' + {$I %DATE%} + ' ' + {$I %TIME%};
  CCompiler = 'Free Pascal ' + {$I %FPCVERSION%};
  CAppInfo = CAppName + ' ' + CAppVersion + ' ' + CBuild + ' ' + CCompiler;
