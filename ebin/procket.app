{application, procket,
    [
    {description, "Raw socket support"},
    {vsn, "0.01"},
    {modules, [
        procket,
        packet,
        mktmp,
        icmp,
        echo
            ]},
    {registered, []},
    {applications, [
        kernel,
        stdlib
            ]},
    {env, []}
    ]}.

