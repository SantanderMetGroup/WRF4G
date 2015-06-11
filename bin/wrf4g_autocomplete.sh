
_wrf4g()
{
    local cur
    cur="${COMP_WORDS[COMP_CWORD]}"

    if [ $COMP_CWORD -eq 1 ]; then
        COMPREPLY=( $( compgen -W '--version -h --help status resource exp stop job vcp start host conf rea id' -- $cur) )
    else
        case ${COMP_WORDS[1]} in
            status)
            _wrf4g_status
        ;;
            resource)
            _wrf4g_resource
        ;;
            exp)
            _wrf4g_exp
        ;;
            stop)
            _wrf4g_stop
        ;;
            job)
            _wrf4g_job
        ;;
            vcp)
            _wrf4g_vcp
        ;;
            start)
            _wrf4g_start
        ;;
            host)
            _wrf4g_host
        ;;
            conf)
            _wrf4g_conf
        ;;
            rea)
            _wrf4g_rea
        ;;
            id)
            _wrf4g_id
        ;;
        esac

    fi
}

_wrf4g_status()
{
    local cur
    cur="${COMP_WORDS[COMP_CWORD]}"

    if [ $COMP_CWORD -ge 2 ]; then
        COMPREPLY=( $( compgen -W '--dbg ' -- $cur) )
    fi
}

_wrf4g_resource()
{
    local cur
    cur="${COMP_WORDS[COMP_CWORD]}"

    if [ $COMP_CWORD -eq 3 ]; then
        COMPREPLY=( $( compgen -W '--dbg edit list check' -- $cur) )
    else
        case ${COMP_WORDS[3]} in
            edit)
            _wrf4g_resource_edit
        ;;
            list)
            _wrf4g_resource_list
        ;;
            check)
            _wrf4g_resource_check
        ;;
        esac

    fi
}

_wrf4g_resource_edit()
{
    local cur
    cur="${COMP_WORDS[COMP_CWORD]}"

    if [ $COMP_CWORD -ge 4 ]; then
        COMPREPLY=( $( compgen -W ' ' -- $cur) )
    fi
}

_wrf4g_resource_list()
{
    local cur
    cur="${COMP_WORDS[COMP_CWORD]}"

    if [ $COMP_CWORD -ge 4 ]; then
        COMPREPLY=( $( compgen -W ' ' -- $cur) )
    fi
}

_wrf4g_resource_check()
{
    local cur
    cur="${COMP_WORDS[COMP_CWORD]}"

    if [ $COMP_CWORD -ge 4 ]; then
        COMPREPLY=( $( compgen -W ' ' -- $cur) )
    fi
}

_wrf4g_exp()
{
    local cur
    cur="${COMP_WORDS[COMP_CWORD]}"

    if [ $COMP_CWORD -eq 3 ]; then
        COMPREPLY=( $( compgen -fW ' status edit stop create list update submit delete define' -- $cur) )
    else
        case ${COMP_WORDS[3]} in
            status)
            _wrf4g_exp_status
        ;;
            edit)
            _wrf4g_exp_edit
        ;;
            stop)
            _wrf4g_exp_stop
        ;;
            create)
            _wrf4g_exp_create
        ;;
            list)
            _wrf4g_exp_list
        ;;
            update)
            _wrf4g_exp_update
        ;;
            submit)
            _wrf4g_exp_submit
        ;;
            delete)
            _wrf4g_exp_delete
        ;;
            define)
            _wrf4g_exp_define
        ;;
        esac

    fi
}

_wrf4g_exp_status()
{
    local cur
    cur="${COMP_WORDS[COMP_CWORD]}"

    if [ $COMP_CWORD -ge 3 ]; then
        COMPREPLY=( $( compgen -W '--dbg -p= --pattern=' -- $cur) )
    fi
}

_wrf4g_exp_edit()
{
    local cur
    cur="${COMP_WORDS[COMP_CWORD]}"

    if [ $COMP_CWORD -ge 3 ]; then
        COMPREPLY=( $( compgen -W '--dbg ' -- $cur) )
    fi
}

_wrf4g_exp_stop()
{
    local cur
    cur="${COMP_WORDS[COMP_CWORD]}"

    if [ $COMP_CWORD -ge 3 ]; then
        COMPREPLY=( $( compgen -W '--dbg -n --dry-run ' -- $cur) )
    fi
}

_wrf4g_exp_create()
{
    local cur
    cur="${COMP_WORDS[COMP_CWORD]}"

    if [ $COMP_CWORD -ge 3 ]; then
        COMPREPLY=( $( compgen -W '--dbg -n --dry-run -d= --dir=' -- $cur) )
    fi
}

_wrf4g_exp_list()
{
    local cur
    cur="${COMP_WORDS[COMP_CWORD]}"

    if [ $COMP_CWORD -ge 4 ]; then
        COMPREPLY=( $( compgen -W '-p= --pattern=' -- $cur) )
    fi
}

_wrf4g_exp_update()
{
    local cur
    cur="${COMP_WORDS[COMP_CWORD]}"

    if [ $COMP_CWORD -ge 4 ]; then
        COMPREPLY=( $( compgen -W '--dbg -n --dry-run -d= --dir=' -- $cur) )
    fi
}

_wrf4g_exp_submit()
{
    local cur
    cur="${COMP_WORDS[COMP_CWORD]}"

    if [ $COMP_CWORD -ge 4 ]; then
        COMPREPLY=( $( compgen -W '--dbg -n --dry-run --rerun ' -- $cur) )
    fi
}

_wrf4g_exp_delete()
{
    local cur
    cur="${COMP_WORDS[COMP_CWORD]}"

    if [ $COMP_CWORD -ge 4 ]; then
        COMPREPLY=( $( compgen -W '--dbg -n --dry-run ' -- $cur) )
    fi
}

_wrf4g_exp_define()
{
    local cur
    cur="${COMP_WORDS[COMP_CWORD]}"

    if [ $COMP_CWORD -ge 4 ]; then
        COMPREPLY=( $( compgen -W '--dbg -f --force -t= --from-template= -d= --dir=' -- $cur) )
    fi
}

_wrf4g_stop()
{
    local cur
    cur="${COMP_WORDS[COMP_CWORD]}"

    if [ $COMP_CWORD -ge 2 ]; then
        COMPREPLY=( $( compgen -W '--dbg ' -- $cur) )
    fi
}

_wrf4g_job()
{
    local cur
    cur="${COMP_WORDS[COMP_CWORD]}"

    if [ $COMP_CWORD -eq 2 ]; then
        COMPREPLY=( $( compgen -W ' cancel list log submit history' -- $cur) )
    else
        case ${COMP_WORDS[2]} in
            cancel)
            _wrf4g_job_cancel
        ;;
            list)
            _wrf4g_job_list
        ;;
            log)
            _wrf4g_job_log
        ;;
            submit)
            _wrf4g_job_submit
        ;;
            history)
            _wrf4g_job_history
        ;;
        esac

    fi
}

_wrf4g_job_cancel()
{
    local cur
    cur="${COMP_WORDS[COMP_CWORD]}"

    if [ $COMP_CWORD -ge 3 ]; then
        COMPREPLY=( $( compgen -fW '--dbg ' -- $cur) )
    fi
}

_wrf4g_job_list()
{
    local cur
    cur="${COMP_WORDS[COMP_CWORD]}"

    if [ $COMP_CWORD -ge 3 ]; then
        COMPREPLY=( $( compgen -fW '--dbg ' -- $cur) )
    fi
}

_wrf4g_job_log()
{
    local cur
    cur="${COMP_WORDS[COMP_CWORD]}"

    if [ $COMP_CWORD -ge 3 ]; then
        COMPREPLY=( $( compgen -fW '--dbg ' -- $cur) )
    fi
}

_wrf4g_job_submit()
{
    local cur
    cur="${COMP_WORDS[COMP_CWORD]}"

    if [ $COMP_CWORD -ge 3 ]; then
        COMPREPLY=( $( compgen -fW '--dbg --dep=' -- $cur) )
    fi
}

_wrf4g_job_history()
{
    local cur
    cur="${COMP_WORDS[COMP_CWORD]}"

    if [ $COMP_CWORD -ge 3 ]; then
        COMPREPLY=( $( compgen -fW '--dbg ' -- $cur) )
    fi
}

_wrf4g_vcp()
{
    local cur
    cur="${COMP_WORDS[COMP_CWORD]}"

    if [ $COMP_CWORD -ge 2 ]; then
        COMPREPLY=( $( compgen -fW '--dbg -o --overwrite ' -- $cur) )
    fi
}

_wrf4g_start()
{
    local cur
    cur="${COMP_WORDS[COMP_CWORD]}"

    if [ $COMP_CWORD -ge 2 ]; then
        COMPREPLY=( $( compgen -W '--dbg --clear-conf --disc-jobs --ext-db --db-port= --db-host=' -- $cur) )
    fi
}

_wrf4g_host()
{
    local cur
    cur="${COMP_WORDS[COMP_CWORD]}"

    if [ $COMP_CWORD -eq 2 ]; then
        COMPREPLY=( $( compgen -fW '--dbg list' -- $cur) )
    else
        case ${COMP_WORDS[2]} in
            list)
            _wrf4g_host_list
        ;;
        esac

    fi
}

_wrf4g_host_list()
{
    local cur
    cur="${COMP_WORDS[COMP_CWORD]}"

    if [ $COMP_CWORD -ge 3 ]; then
        COMPREPLY=( $( compgen -W ' ' -- $cur) )
    fi
}

_wrf4g_conf()
{
    local cur
    cur="${COMP_WORDS[COMP_CWORD]}"

    if [ $COMP_CWORD -eq 2 ]; then
        COMPREPLY=( $( compgen -W '--dbg daemon sched logger database' -- $cur) )
    else
        case ${COMP_WORDS[2]} in
            daemon)
            _wrf4g_conf_daemon
        ;;
            sched)
            _wrf4g_conf_sched
        ;;
            logger)
            _wrf4g_conf_logger
        ;;
            database)
            _wrf4g_conf_database
        ;;
        esac

    fi
}

_wrf4g_conf_daemon()
{
    local cur
    cur="${COMP_WORDS[COMP_CWORD]}"

    if [ $COMP_CWORD -ge 3 ]; then
        COMPREPLY=( $( compgen -W ' ' -- $cur) )
    fi
}

_wrf4g_conf_sched()
{
    local cur
    cur="${COMP_WORDS[COMP_CWORD]}"

    if [ $COMP_CWORD -ge 3 ]; then
        COMPREPLY=( $( compgen -W ' ' -- $cur) )
    fi
}

_wrf4g_conf_logger()
{
    local cur
    cur="${COMP_WORDS[COMP_CWORD]}"

    if [ $COMP_CWORD -ge 3 ]; then
        COMPREPLY=( $( compgen -W ' ' -- $cur) )
    fi
}

_wrf4g_conf_database()
{
    local cur
    cur="${COMP_WORDS[COMP_CWORD]}"

    if [ $COMP_CWORD -ge 3 ]; then
        COMPREPLY=( $( compgen -W ' ' -- $cur) )
    fi
}

_wrf4g_rea()
{
    local cur
    cur="${COMP_WORDS[COMP_CWORD]}"

    if [ $COMP_CWORD -eq 3 ]; then
        COMPREPLY=( $( compgen -fW ' status stop submit log' -- $cur) )
    else
        case ${COMP_WORDS[3]} in
            status)
            _wrf4g_rea_status
        ;;
            stop)
            _wrf4g_rea_stop
        ;;
            submit)
            _wrf4g_rea_submit
        ;;
            log)
            _wrf4g_rea_log
        ;;
        esac

    fi
}

_wrf4g_rea_status()
{
    local cur
    cur="${COMP_WORDS[COMP_CWORD]}"

    if [ $COMP_CWORD -ge 4 ]; then
        COMPREPLY=( $( compgen -W '--dbg ' -- $cur) )
    fi
}

_wrf4g_rea_stop()
{
    local cur
    cur="${COMP_WORDS[COMP_CWORD]}"

    if [ $COMP_CWORD -ge 4 ]; then
        COMPREPLY=( $( compgen -W '--dbg -n --dry-run ' -- $cur) )
    fi
}

_wrf4g_rea_submit()
{
    local cur
    cur="${COMP_WORDS[COMP_CWORD]}"

    if [ $COMP_CWORD -ge 4 ]; then
        COMPREPLY=( $( compgen -fW '--dbg -n --dry-run --rerun ' -- $cur) )
    fi
}

_wrf4g_rea_log()
{
    local cur
    cur="${COMP_WORDS[COMP_CWORD]}"

    if [ $COMP_CWORD -ge 4 ]; then
        COMPREPLY=( $( compgen -fW '--dbg -d= --dir=' -- $cur) )
    fi
}


_wrf4g_id()
{
    local cur
    cur="${COMP_WORDS[COMP_CWORD]}"

    if [ $COMP_CWORD -eq 3 ]; then
        COMPREPLY=( $( compgen -fW ' info init delete' -- $cur) )
    else
        case ${COMP_WORDS[3]} in
            info)
            _wrf4g_id_info
        ;;
            init)
            _wrf4g_id_init
        ;;
            delete)
            _wrf4g_id_delete
        ;;
        esac

    fi
}

_wrf4g_id_info()
{
    local cur
    cur="${COMP_WORDS[COMP_CWORD]}"

    if [ $COMP_CWORD -ge 4 ]; then
        COMPREPLY=( $( compgen -W '--dbg ' -- $cur) )
    fi
}

_wrf4g_id_init()
{
    local cur
    cur="${COMP_WORDS[COMP_CWORD]}"

    if [ $COMP_CWORD -ge 4 ]; then
        COMPREPLY=( $( compgen -W '--dbg -l= --lifetime=' -- $cur) )
    fi
}

_wrf4g_id_delete()
{
    local cur
    cur="${COMP_WORDS[COMP_CWORD]}"

    if [ $COMP_CWORD -ge 4 ]; then
        COMPREPLY=( $( compgen -W '--dbg ' -- $cur) )
    fi
}

complete -F _wrf4g wrf4g
