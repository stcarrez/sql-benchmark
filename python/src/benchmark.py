#!/usr/bin/env python3
"""
Benchmark class
"""
__author__ = "Stephane Carrez"
__copyright__ = "Copyright (C) 2018 Stephane Carrez"
__license__ = 'Apache License, Version 2.0'

import time
import traceback
import configparser

"""
Benchmark context information.

This class collects benchmark information in static properties and provides
operations to print the final results in XML.
"""
class Benchmark:
    """
    The results collected when running the benchmark.
    """
    _results = {}

    """
    The name of the database driver used to run the benchmark.
    """
    _driver = ''

    """
    The configuration properties.
    """
    _config = {}

    """
    The base repeat counter configured with -r option.
    """
    _repeat_base = 10

    def __init__(self):
        self.repeat_factor = 100

    @staticmethod
    def read_config(path):
        try:
            with open(path) as f:
                for line in f:
                    l = line.strip()
                    if l and not l.startswith('#'):
                        items = l.split('=')
                        if len(items) > 0:
                            name = items[0].strip()
                            value = '='.join(items[1:]).strip().strip('"')
                            Benchmark._config[name] = value
        except:
            print("Cannot read {0}".format(path))

    @staticmethod
    def read_process_info():
        thread_count = 0
        rss_size = 0
        hwm_size = 0
        sys_time = 0
        user_time = 0
        try:
            with open("/proc/self/status") as f:
                for line in f:
                    line = line.strip()
                    items = line.split()
                    if line.startswith("Threads:") and len(items) > 0:
                        thread_count = int(items[1])
                    elif line.startswith("VmRSS:") and len(items) > 0:
                        rss_size = int(items[1])
                    elif line.startswith("VmHWM:") and len(items) > 0:
                        hwm_size = int(items[1])
        except:
            pass

        try:
            with open("/proc/self/stat") as f:
                for line in f:
                    line = line.strip()
                    items = line.split()
                    if len(items) > 17:
                        user_time = 10 * int(items[13])
                        sys_time = 10 * int(items[14])
        except:
            pass

        print("<benchmark language='Python'", end='')
        print(" driver='{0}' threads='{1}'".format(Benchmark._driver, thread_count), end='')
        print(" rss_size='{0}' peek_rss_size='{1}'".format(rss_size, hwm_size), end='')
        print(" user_time='{0}' sys_time='{1}'".format(user_time, sys_time), end='')
        print(">")

    @staticmethod
    def format_time(t):
        us = int(t * 1000000.0)
        ms = int(us / 1000)
        if ms > 1000:
            sec = int(ms / 1000)
            ms = ms % 1000
            return "{0}.{1:03d} s".format(sec, ms)
        if ms > 100:
            us = us % 1000
            return "{0}.{1} ms".format(ms, int(us / 100))
        if ms > 10:
            us = us % 1000
            return "{0}.{1:02d} ms".format(ms, int(us / 10))
        if ms > 1:
            us = us % 1000
            return "{0}.{1:03d} ms".format(ms, us)
        return "{0} us".format(us)

    @staticmethod
    def print_report():
        Benchmark.read_process_info()
        print("<measures title='SQL Benchmark'>")
        for title, result in Benchmark._results.items():
            t = result['time']
            n = result['repeat']
            print("<time count='{0}' time='{1}' total='{2}' title='{3}'/>"
            .format(n, Benchmark.format_time(t / n), Benchmark.format_time(t), title))
        print("</measures>")
        print("</benchmark>")

    def repeat(self):
        return Benchmark._repeat_base * self.repeat_factor

    def run(self):
        repeat = self.repeat()
        
        start = time.time()
        try:
            self.execute()
        except Exception as ex:
            print("Exception: ")
            print(ex)
            traceback.print_exc()

        end = time.time()
        dt = end - start
        Benchmark._results[self.title] = { 'repeat': repeat, 'time': dt }




