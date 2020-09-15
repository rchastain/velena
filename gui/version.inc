
const
  CAppVersion = '0.1.0';
  CAppName = 'Velena GUI';
  CBuild = 'build ' + {$I %DATE%} + ' ' + {$I %TIME%};
  CCompiler = 'Free Pascal ' + {$I %FPCVERSION%};
  CAppInfo = CAppName + ' ' + CAppVersion + ' ' + CBuild + ' ' + CCompiler;
