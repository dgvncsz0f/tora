{-# LANGUAGE OverloadedStrings #-}

module Tora.Templates
  ( findTemplate
  , findSearchPath
  , findMappingPath
  )where

import           Data.Time.LocalTime
import           Tora.PPrint

findTemplate :: String -> Maybe (TimeZone -> Template)
findTemplate "lukla-production" = findTemplate "syslog"
findTemplate "lukla-staging" = findTemplate "syslog"
findTemplate "renminbi-staging" = findTemplate "syslog"
findTemplate "renminbi-production" = findTemplate "syslog"
findTemplate "syslog" =
  Just $ \timezone -> compile defaultOpts
    $ Data (fetchSeverity ["_source", "syslog", "severity"])
    $ Data (fetchTimestamp timezone ["_source", "@timestamp"])
    $ Data (fetchData ["_index"])
    $ Data (fetchData ["_id"])
    $ Data (fetchData ["_source", "syslog", "host"])
    $ Data (fetchData ["_source", "syslog", "app-name"])
    $ Data (fetchData ["_source", "syslog", "procid"])
    $ StyleT (Nest 4)
    $ StyleT BreakLine
    $ Data (fetchData ["_source", "msg"])
    $ Done
findTemplate "osquery-logged-in-users" =
  Just $ \timezone -> compile defaultOpts
    $ Data (fetchTimestamp timezone ["_source", "@timestamp"])
    $ Data (fetchData ["_source", "beat", "host"])
    $ Data (fetchData ["_index"])
    $ Data (fetchData ["_id"])
    $ StyleT (Nest 4)
    $ StyleT BreakLine
    $ Data (fetchData ["_source", "json", "columns", "host"])
    $ Data (fetchData ["_source", "json", "columns", "user"])
    $ Data (fetchData ["_source", "json", "columns", "type"])
    $ StyleT BreakLine
    $ Data (fetchData ["_source", "json", "columns", "cmdline"])
    $ Done
findTemplate "osquery-shell-history" =
  Just $ \timezone -> compile defaultOpts
    $ Data (fetchTimestamp timezone ["_source", "@timestamp"])
    $ Data (fetchData ["_source", "host", "name"])
    $ Data (fetchData ["_index"])
    $ Data (fetchData ["_id"])
    $ StyleT (Nest 4)
    $ StyleT BreakLine
    $ Data (fetchData ["_source", "json", "columns", "username"])
    $ Data (fetchData ["_source", "json", "columns", "directory"])
    $ StyleT BreakLine
    $ Data (fetchData ["_source", "json", "columns", "command"])
    $ Done
findTemplate _ = error "Tora.Templates#findTemplate"

findSearchPath :: String -> Maybe String
findSearchPath "syslog" = Just "/logstash-syslog-*/_search"
findSearchPath "lukla-staging" = Just "/logstash-xerpa-lukla-staging-*/_search"
findSearchPath "lukla-production" = Just "/logstash-xerpa-lukla-production-*/_search"
findSearchPath "renminbi-staging" = Just "/logstash-xerpa-renminbi-staging-*/_search"
findSearchPath "renminbi-production" = Just "/logstash-xerpa-renminbi-production-*/_search"
findSearchPath "osquery-logged-in-users" = Just "/logstash-osquery-pack_incident-response_logged_in_users-*/_search"
findSearchPath "osquery-shell-history" = Just "/logstash-osquery-pack_incident-response_shell_history-*/_search"
findSearchPath _ = Nothing

findMappingPath :: String -> Maybe String
findMappingPath name = Just $ "/" <> name
