{application, procket,
    [
    {description, "Low level socket operations"},
    {vsn, "0.03"},
    {modules, [
        procket,
        procket_ioctl,
        packet,
        bpf,
        tuntap,
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

