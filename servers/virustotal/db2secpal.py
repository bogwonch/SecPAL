#!/usr/bin/env python
# encoding: utf-8
import sqlite3

conn = sqlite3.connect('virustotal.sqlite')
c = conn.cursor()

c.execute("""
    select name, checker
      from app 
      join virustotal 
        on app.hash = virustotal.hash 
     where result is not null;
     """,[])
results = c.fetchall()

for name, checker in results:
    print checker.title()+" says apk#"+name.upper()+" is-malware;"
