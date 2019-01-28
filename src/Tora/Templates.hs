{-# LANGUAGE OverloadedStrings #-}

module Tora.Templates where

import qualified Data.Text.Lazy      as TL
import           Data.Time.Format
import           Data.Time.LocalTime
import           Tora.PPrint

findTemplate :: String -> Maybe (TimeZone -> Template)
findTemplate "lukla-production" = findTemplate "syslog"
findTemplate "lukla-staging" = findTemplate "syslog"
findTemplate "renminbi-staging" = findTemplate "syslog"
findTemplate "renminbi-production" = findTemplate "syslog"
findTemplate "syslog" =
  Just $ \timezone -> compile defaultOpts $
    Data (fetchSeverity ["_source", "syslog", "severity"])
    $ Data (fetchTimestamp defaultTimeLocale timezone ["_source", "@timestamp"])
    $ Data (fetchData ["_index"])
    $ Data (fetchData ["_id"])
    $ Data (fetchData ["_source", "syslog", "host"])
    $ Data (fetchData ["_source", "syslog", "app-name"])
    $ Data (fetchData ["_source", "syslog", "procid"])
    $ StyleT (Nest 4)
    $ StyleT BreakLine
    $ Data (fetchData ["_source", "msg"])
    $ Done
findTemplate _ = Nothing

findSearchPath :: String -> Maybe String
findSearchPath "syslog"              = Just "/logstash-syslog-*/_search"
findSearchPath "lukla-staging"       = Just "/logstash-xerpa-lukla-staging-*/_search"
findSearchPath "lukla-production"    = Just "/logstash-xerpa-lukla-production-*/_search"
findSearchPath "renminbi-staging"    = Just "/logstash-xerpa-renminbi-staging-*/_search"
findSearchPath "renminbi-production" = Just "/logstash-xerpa-renminbi-production-*/_search"
findSearchPath _ = Nothing
