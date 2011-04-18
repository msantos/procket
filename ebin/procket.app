{application, procket,
    [
    {description, "Low level socket operations"},
    {vsn, "0.03"},
    {modules, [
        procket,
        packet,
        bpf,
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

