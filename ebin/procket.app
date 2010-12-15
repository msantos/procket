{application, procket,
    [
    {description, "Low level socket operations"},
    {vsn, "0.02"},
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

