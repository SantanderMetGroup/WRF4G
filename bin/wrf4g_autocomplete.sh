#
# Copyright 2016 Universidad de Cantabria
#
# Licensed under the EUPL, Version 1.1 only (the
# "Licence");
# You may not use this work except in compliance with the
# Licence.
# You may obtain a copy of the Licence at:
#
# http://ec.europa.eu/idabc/eupl
#
# Unless required by applicable law or agreed to in
# writing, software distributed under the Licence is
# distributed on an "AS IS" basis,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either
# express or implied.
# See the Licence for the specific language governing
# permissions and limitations under the Licence.
#

_wrf4g()
{
    local cur
    cur="${COMP_WORDS[COMP_CWORD]}"

    if [ $COMP_CWORD -eq 1 ]; then
        COMPREPLY=( $( compgen -W '--version -h --help status resource syncdb exp stop job vcp start host conf rea id' -- $cur) )
    else
        case ${COMP_WORDS[1]} in
            status)
            _wrf4g_status
        ;;
            resource)
            _wrf4g_resource
        ;;
            syncdb)
            _wrf4g_syncdb
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
        COMPREPLY=( $( compgen -W '--dbg' -- $cur) )
    fi
}

_wrf4g_resource()
{
    local cur
    cur="${COMP_WORDS[COMP_CWORD]}"
 
    if [ $COMP_CWORD -eq 2 ]; then
        COMPREPLY=( $( compgen -W '--dbg edit list check' -- $cur) )
    fi
}

_wrf4g_syncdb()
{
    local cur
    cur="${COMP_WORDS[COMP_CWORD]}"

    if [ $COMP_CWORD -ge 2 ]; then
        COMPREPLY=( $( compgen -W '--dbg' -- $cur) )
    fi
}

_wrf4g_exp()
{
    local cur
    cur="${COMP_WORDS[COMP_CWORD]}"
 
    if [ $COMP_CWORD -eq 2 ]; then
        COMPREPLY=( $( compgen -W ' list' -- $cur) )
    fi

    if [ $COMP_CWORD -eq 3 ]; then
        COMPREPLY=( $( compgen -W ' status edit cancel create update submit statistics delete define set-priority' -- $cur) )
    else
        case ${COMP_WORDS[3]} in
            status)
            _wrf4g_exp_status
        ;;
            edit)
            _wrf4g_exp_edit
        ;;
            cancel)
            _wrf4g_exp_cancel
        ;;
            create)
            _wrf4g_exp_create
        ;;
            update)
            _wrf4g_exp_update
        ;;
            submit)
            _wrf4g_exp_submit
        ;;
            statistics)
            _wrf4g_exp_statistics
        ;;
            delete)
            _wrf4g_exp_delete
        ;;
            define)
            _wrf4g_exp_define
        ;;
            set-priority)
            _wrf4g_exp_set_priority
        ;;

        esac

    fi
}

_wrf4g_exp_status()
{
    local cur
    cur="${COMP_WORDS[COMP_CWORD]}"

    if [ $COMP_CWORD -ge 3 ]; then
        COMPREPLY=( $( compgen -W '--dbg -p --pattern -s --rea-state --delay' -- $cur) )
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

_wrf4g_exp_cancel()
{
    local cur
    cur="${COMP_WORDS[COMP_CWORD]}"

    if [ $COMP_CWORD -ge 3 ]; then
        COMPREPLY=( $( compgen -W '--dbg -n --dry-run -p --pattern --hard -s --rea-state' -- $cur) )
    fi
}

_wrf4g_exp_create()
{
    local cur
    cur="${COMP_WORDS[COMP_CWORD]}"

    if [ $COMP_CWORD -ge 3 ]; then
        COMPREPLY=( $( compgen -fW '--dbg -n --dry-run -d --dir' -- $cur) )
    fi
}

_wrf4g_exp_update()
{
    local cur
    cur="${COMP_WORDS[COMP_CWORD]}"

    if [ $COMP_CWORD -ge 4 ]; then
        COMPREPLY=( $( compgen -fW '--dbg -n --dry-run -d --dir' -- $cur) )
    fi
}

_wrf4g_exp_submit()
{
    local cur
    cur="${COMP_WORDS[COMP_CWORD]}"

    if [ $COMP_CWORD -ge 4 ]; then
        COMPREPLY=( $( compgen -W '--dbg -n --dry-run --rerun -p --pattern -s --rea-state -P --priority' -- $cur) )
    fi
}

_wrf4g_exp_statistics()
{
    local cur
    cur="${COMP_WORDS[COMP_CWORD]}"

    if [ $COMP_CWORD -ge 4 ]; then
        COMPREPLY=( $( compgen -W '-p --pattern ' -- $cur) )
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

_wrf4g_exp_set_priority()
{
    local cur
    cur="${COMP_WORDS[COMP_CWORD]}"

    if [ $COMP_CWORD -ge 4 ]; then
        COMPREPLY=( $( compgen -W '--dbg -n --dry-run -p --pattern' -- $cur) )
    fi
}

_wrf4g_exp_define()
{
    local cur
    cur="${COMP_WORDS[COMP_CWORD]}"

    if [ $COMP_CWORD -ge 4 ]; then
        COMPREPLY=( $( compgen -fW '--dbg -f --force -t --from-template -d --dir' -- $cur) )
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
        COMPREPLY=( $( compgen -W ' cancel list log submit history ' -- $cur) )
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
        COMPREPLY=( $( compgen -W '--dbg --hard' -- $cur) )
    fi
}

_wrf4g_job_list()
{
    local cur
    cur="${COMP_WORDS[COMP_CWORD]}"

    if [ $COMP_CWORD -ge 3 ]; then
        COMPREPLY=( $( compgen -W '--dbg --delay' -- $cur) )
    fi
}

_wrf4g_job_log()
{
    local cur
    cur="${COMP_WORDS[COMP_CWORD]}"

    if [ $COMP_CWORD -ge 3 ]; then
        COMPREPLY=( $( compgen -W '--dbg ' -- $cur) )
    fi
}

_wrf4g_job_submit()
{
    local cur
    cur="${COMP_WORDS[COMP_CWORD]}"

    if [ $COMP_CWORD -ge 3 ]; then
        COMPREPLY=( $( compgen -W '--dbg --dep' -- $cur) )
    fi
}

_wrf4g_job_history()
{
    local cur
    cur="${COMP_WORDS[COMP_CWORD]}"

    if [ $COMP_CWORD -ge 3 ]; then
        COMPREPLY=( $( compgen -W '--dbg' -- $cur) )
    fi
}

_wrf4g_vcp()
{
    local cur
    cur="${COMP_WORDS[COMP_CWORD]}"

    if [ $COMP_CWORD -ge 2 ]; then
        COMPREPLY=( $( compgen -fW '--dbg -o --overwrite' -- $cur) )
    fi
}

_wrf4g_start()
{
    local cur
    cur="${COMP_WORDS[COMP_CWORD]}"

    if [ $COMP_CWORD -ge 2 ]; then
        COMPREPLY=( $( compgen -W '--dbg --clear-conf --disc-jobs --ext-db --db-port --db-host' -- $cur) )
    fi
}

_wrf4g_host()
{
    local cur
    cur="${COMP_WORDS[COMP_CWORD]}"

    if [ $COMP_CWORD -eq 2 ]; then
        COMPREPLY=( $( compgen -W '--dbg list') )
    fi
}

_wrf4g_conf()
{
    local cur
    cur="${COMP_WORDS[COMP_CWORD]}"

    if [ $COMP_CWORD -eq 2 ]; then
        COMPREPLY=( $( compgen -W '--dbg daemon sched logger database' -- $cur) )
    fi 
}

_wrf4g_rea()
{
    local cur
    cur="${COMP_WORDS[COMP_CWORD]}"

    if [ $COMP_CWORD -eq 3 ]; then
        COMPREPLY=( $( compgen -W 'status cancel submit set-priority set-restart get-restart info log ' -- $cur) )
    else
        case ${COMP_WORDS[3]} in
            status)
            _wrf4g_rea_status
        ;;
            cancel)
            _wrf4g_rea_cancel
        ;;
            submit)
            _wrf4g_rea_submit
        ;;
            set-priority)
            _wrf4g_rea_set_priority
        ;;
           set-restart)
            _wrf4g_rea_set_restart
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
        COMPREPLY=( $( compgen -W '--dbg --delay' -- $cur) )
    fi
}

_wrf4g_rea_cancel()
{
    local cur
    cur="${COMP_WORDS[COMP_CWORD]}"

    if [ $COMP_CWORD -ge 4 ]; then
        COMPREPLY=( $( compgen -W '--dbg -n --dry-run --hard' -- $cur) )
    fi
}

_wrf4g_rea_submit()
{
    local cur
    cur="${COMP_WORDS[COMP_CWORD]}"

    if [ $COMP_CWORD -ge 4 ]; then
        COMPREPLY=( $( compgen -W '--dbg -n --dry-run --rerun -P --priority' -- $cur) )
    fi
}

_wrf4g_rea_set_priority()
{
    local cur
    cur="${COMP_WORDS[COMP_CWORD]}"

    if [ $COMP_CWORD -ge 4 ]; then
        COMPREPLY=( $( compgen -W '--dbg -n --dry-run ' -- $cur) )
    fi
}

_wrf4g_rea_set_restart()
{
    local cur
    cur="${COMP_WORDS[COMP_CWORD]}"

    if [ $COMP_CWORD -ge 4 ]; then
        COMPREPLY=( $( compgen -W '--dbg ' -- $cur) )
    fi
}

_wrf4g_rea_log()
{
    local cur
    cur="${COMP_WORDS[COMP_CWORD]}"

    if [ $COMP_CWORD -ge 4 ]; then
        COMPREPLY=( $( compgen -fW '--dbg -d --dir' -- $cur) )
    fi
}


_wrf4g_id()
{
    local cur
    cur="${COMP_WORDS[COMP_CWORD]}"

    if [ $COMP_CWORD -eq 3 ]; then
        COMPREPLY=( $( compgen -W 'info init delete' -- $cur) )
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
        COMPREPLY=( $( compgen -W '--dbg' -- $cur) )
    fi
}

_wrf4g_id_init()
{
    local cur
    cur="${COMP_WORDS[COMP_CWORD]}"

    if [ $COMP_CWORD -ge 4 ]; then
        COMPREPLY=( $( compgen -W '--dbg -l --lifetime' -- $cur) )
    fi
}

_wrf4g_id_delete()
{
    local cur
    cur="${COMP_WORDS[COMP_CWORD]}"

    if [ $COMP_CWORD -ge 4 ]; then
        COMPREPLY=( $( compgen -W '--dbg' -- $cur) )
    fi
}

complete -F _wrf4g wrf4g
