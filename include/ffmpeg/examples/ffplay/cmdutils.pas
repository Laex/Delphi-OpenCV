unit cmdutils;

interface

implementation

procedure parse_loglevel(const options:pOptionDef);
Var
 idx:Integer;
 env:pAnsiChar;
 i:Integer;
begin
    idx := locate_option(options, 'loglevel');
    if (idx=0) then
        idx := locate_option(options, 'v');
    if (idx<>0) and (ParamCount>idx) then
        opt_loglevel(nil, 'loglevel', ParamStr(idx+1)));
    idx = locate_option(options, 'report');
    env := getenv('FFREPORT');
    if Assigned(env) or (idx<>0) then
    begin
        init_report(env);
        if Assigned(report_file) then
        begin

            fprintf(report_file, 'Command line:\n');
            for (i = 0; i < argc; i++) {
                dump_argument(argv[i]);
                fputc(i < argc - 1 ? ' ' : '\n', report_file);
            end;
            fflush(report_file);
        end;
    end;
end;

end.
