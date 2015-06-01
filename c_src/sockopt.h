#include <string.h>

int level_lookup(char *level_name, int level_size, int *level_value)
{
    if (level_size == 10)
    {
        if (strncmp("SOL_SOCKET", level_name, level_size) == 0)
        {
            *level_value = SOL_SOCKET;
            return 1;
        }
        else if (strncmp("IPPROTO_IP", level_name, level_size) == 0)
        {
            *level_value = IPPROTO_IP;
            return 1;
        }
        else if (strncmp("IPPROTO_TP", level_name, level_size) == 0)
        {
            *level_value = IPPROTO_TP;
            return 1;
        }
        else if (strncmp("IPPROTO_AH", level_name, level_size) == 0)
        {
            *level_value = IPPROTO_AH;
            return 1;
        }
    }
    else if (level_size == 11)
    {
        if (strncmp("IPPROTO_TCP", level_name, level_size) == 0)
        {
            *level_value = IPPROTO_TCP;
            return 1;
        }
        else if (strncmp("IPPROTO_EGP", level_name, level_size) == 0)
        {
            *level_value = IPPROTO_EGP;
            return 1;
        }
        else if (strncmp("IPPROTO_PUP", level_name, level_size) == 0)
        {
            *level_value = IPPROTO_PUP;
            return 1;
        }
        else if (strncmp("IPPROTO_UDP", level_name, level_size) == 0)
        {
            *level_value = IPPROTO_UDP;
            return 1;
        }
        else if (strncmp("IPPROTO_IDP", level_name, level_size) == 0)
        {
            *level_value = IPPROTO_IDP;
            return 1;
        }
        else if (strncmp("IPPROTO_GRE", level_name, level_size) == 0)
        {
            *level_value = IPPROTO_GRE;
            return 1;
        }
        else if (strncmp("IPPROTO_ESP", level_name, level_size) == 0)
        {
            *level_value = IPPROTO_ESP;
            return 1;
        }
        else if (strncmp("IPPROTO_MTP", level_name, level_size) == 0)
        {
            *level_value = IPPROTO_MTP;
            return 1;
        }
        else if (strncmp("IPPROTO_PIM", level_name, level_size) == 0)
        {
            *level_value = IPPROTO_PIM;
            return 1;
        }
        else if (strncmp("IPPROTO_RAW", level_name, level_size) == 0)
        {
            *level_value = IPPROTO_RAW;
            return 1;
        }
    }
    else if (level_size == 12)
    {
        if (strncmp("IPPROTO_IPV6", level_name, level_size) == 0)
        {
            *level_value = IPPROTO_IPV6;
            return 1;
        }
        else if (strncmp("IPPROTO_ICMP", level_name, level_size) == 0)
        {
            *level_value = IPPROTO_ICMP;
            return 1;
        }
        else if (strncmp("IPPROTO_IGMP", level_name, level_size) == 0)
        {
            *level_value = IPPROTO_IGMP;
            return 1;
        }
        else if (strncmp("IPPROTO_IPIP", level_name, level_size) == 0)
        {
            *level_value = IPPROTO_IPIP;
            return 1;
        }
        else if (strncmp("IPPROTO_DCCP", level_name, level_size) == 0)
        {
            *level_value = IPPROTO_DCCP;
            return 1;
        }
        else if (strncmp("IPPROTO_RSVP", level_name, level_size) == 0)
        {
            *level_value = IPPROTO_RSVP;
            return 1;
        }
        else if (strncmp("IPPROTO_COMP", level_name, level_size) == 0)
        {
            *level_value = IPPROTO_COMP;
            return 1;
        }
        else if (strncmp("IPPROTO_SCTP", level_name, level_size) == 0)
        {
            *level_value = IPPROTO_SCTP;
            return 1;
        }
    }
    else if (level_size == 13)
    {
        if (strncmp("IPPROTO_ENCAP", level_name, level_size) == 0)
        {
            *level_value = IPPROTO_ENCAP;
            return 1;
        }
    }
    else if (level_size == 14)
    {
#ifdef IPPROTO_BEETPH
        if (strncmp("IPPROTO_BEETPH", level_name, level_size) == 0)
        {
            *level_value = IPPROTO_BEETPH;
            return 1;
        }
#endif
    }
    else if (level_size == 15)
    {
        if (strncmp("IPPROTO_UDPLITE", level_name, level_size) == 0)
        {
            *level_value = IPPROTO_UDPLITE;
            return 1;
        }
    }

    *level_value = -1;
    return 0;
}

int optname_lookup(char *option_name, int option_size, int *option_value)
{
    if (option_size == 7)
    {
        if (strncmp("SO_TYPE", option_name, option_size) == 0)
        {
            *option_value = SO_TYPE;
            return 1;
        }
    }
    else if (option_size == 8)
    {
        if (strncmp("SO_DEBUG", option_name, option_size) == 0)
        {
            *option_value = SO_DEBUG;
            return 1;
        }
        else if (strncmp("SO_ERROR", option_name, option_size) == 0)
        {
            *option_value = SO_ERROR;
            return 1;
        }
    }
    else if (option_size == 9)
    {
        if (strncmp("SO_SNDBUF", option_name, option_size) == 0)
        {
            *option_value = SO_SNDBUF;
            return 1;
        }
        else if (strncmp("SO_RCVBUF", option_name, option_size) == 0)
        {
            *option_value = SO_RCVBUF;
            return 1;
        }
        else if (strncmp("SO_LINGER", option_name, option_size) == 0)
        {
            *option_value = SO_LINGER;
            return 1;
        }
    }
    else if (option_size == 11)
    {
        if (strncmp("SO_PASSCRED", option_name, option_size) == 0)
        {
            *option_value = SO_PASSCRED;
            return 1;
        }
        else if (strncmp("SO_PEERCRED", option_name, option_size) == 0)
        {
            *option_value = SO_PEERCRED;
            return 1;
        }
        else if (strncmp("SO_RCVLOWAT", option_name, option_size) == 0)
        {
            *option_value = SO_RCVLOWAT;
            return 1;
        }
        else if (strncmp("SO_SNDLOWAT", option_name, option_size) == 0)
        {
            *option_value = SO_SNDLOWAT;
            return 1;
        }
        else if (strncmp("SO_RCVTIMEO", option_name, option_size) == 0)
        {
            *option_value = SO_RCVTIMEO;
            return 1;
        }
        else if (strncmp("SO_SNDTIMEO", option_name, option_size) == 0)
        {
            *option_value = SO_SNDTIMEO;
            return 1;
        }
        else if (strncmp("SO_NO_CHECK", option_name, option_size) == 0)
        {
            *option_value = SO_NO_CHECK;
            return 1;
        }
        else if (strncmp("SO_PRIORITY", option_name, option_size) == 0)
        {
            *option_value = SO_PRIORITY;
            return 1;
        }
    }
    else if (option_size == 12)
    {
        if (strncmp("SO_REUSEADDR", option_name, option_size) == 0)
        {
            *option_value = SO_REUSEADDR;
            return 1;
        }
        else if (strncmp("SO_DONTROUTE", option_name, option_size) == 0)
        {
            *option_value = SO_DONTROUTE;
            return 1;
        }
        else if (strncmp("SO_BROADCAST", option_name, option_size) == 0)
        {
            *option_value = SO_BROADCAST;
            return 1;
        }
        else if (strncmp("SO_KEEPALIVE", option_name, option_size) == 0)
        {
            *option_value = SO_KEEPALIVE;
            return 1;
        }
        else if (strncmp("SO_OOBINLINE", option_name, option_size) == 0)
        {
            *option_value = SO_OOBINLINE;
            return 1;
        }
        else if (strncmp("SO_BSDCOMPAT", option_name, option_size) == 0)
        {
            *option_value = SO_BSDCOMPAT;
            return 1;
        }
#ifdef SO_REUSEPORT
        else if (strncmp("SO_REUSEPORT", option_name, option_size) == 0)
        {
            *option_value = SO_REUSEPORT;
            return 1;
        }
    }
#endif
    else if (option_size == 14)
    {
        if (strncmp("SO_SNDBUFFORCE", option_name, option_size) == 0)
        {
            *option_value = SO_SNDBUFFORCE;
            return 1;
        }
        else if (strncmp("SO_RCVBUFFORCE", option_name, option_size) == 0)
        {
            *option_value = SO_RCVBUFFORCE;
            return 1;
        }
    }
    else if (option_size == 16)
    {
        if (strncmp("IPV6_RECVPKTINFO", option_name, option_size) == 0)
        {
            *option_value = IPV6_RECVPKTINFO;
            return 1;
        }
    }

    *option_value = -1;
    return 0;
}
