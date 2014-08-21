#! /usr/bin/env python
import oursql
import sqlite3
import virustotal
from os import (environ)
from sys import (argv)
import time

class VirusTotalDB:
    _conn = None
    _skb = oursql.connect(
            host='localhost', 
            user='root', 
            db='app_db', 
            port=3306)
    _vt = None

    def __init__(self, db='virustotal.sqlite', api_key=None):
        """Create a new VirusTotal object"""
        self._conn = sqlite3.connect(db)
        
        if not api_key:
            try:
                api_key = environ['VIRUSTOTAL_API']
            except KeyError, e:
                print("""
                    Virustotal API key not given. 
                    Set the VIRUSTOTAL_API environment variable
                """)
                return None

        self._vt = virustotal.VirusTotal(api_key)


    def query(self, app):
        """Query an app against VirusTotal"""
        return self._query_vt_db(app)

    def malware_percentage(self, app):
        """What percentage of checkers think the app is malware?"""
        results = self.query(app)
        if len(results) == 0:
            return 0.0

        positives = 0.0
        for k, v in results.iteritems():
            if v != None:
                positives += 1

        return positives / len(results)


    def _query_vt_db(self, app):
        """Query the local VirusTotal DB""" 
        h = self._hash_app(app)
        c = self._conn.cursor()
        c.execute("""
            SELECT virustotal.checker
                 , virustotal.result
              FROM virustotal
             WHERE virustotal.hash = ?
            ;
            """, [h])

        results = c.fetchall()
        if not results:
            self._update_vt_db(app)
            return self._query_vt_db(app)
        else:
            report = {}
            for checker, result in results:
                report[checker] = result

            return report

    def _update_vt_db(self, app):
        h = self._hash_app(app)
        results = self._vt.get(h)
        while not results.done:
            continue

        c = self._conn.cursor()
        c.execute("""
            INSERT OR REPLACE
            INTO app(name, hash)
            VALUES (?, ?);
        """, [app, h])
        for antivirus, malware in results:
            c.execute("""
                INSERT 
                INTO virustotal(checker, result, hash, time)
                VALUES (?, ?, ?, datetime('now'));
            """, [antivirus[0], malware, h]) 
        self._conn.commit()


    def _hash_app(self, app):
        try:
            return self._hash_app_skb(app)
        except KeyError, e:
            return self._hash_app_file(app)
        raise KeyError("cannot hash app")

    def _hash_app_file(self, app):
        # http://stackoverflow.com/questions/1869885/calculating-sha1-of-a-file
        import hashlib
        sha = hashlib.sha1()
        filename = app.upper()+".apk"
        with open("../../apps/"+filename, 'rb') as f:
            while True:
                block = f.read(2**10) # Magic number: one-megabyte
                if not block: break
                sha.update(block)
            return sha.hexdigest()

    def _hash_app_skb(self, app):
        """Search for an apps hash in the SKB"""
        curs = self._skb.cursor()
        curs.execute("""
            SELECT hashAPK 
              FROM app 
              JOIN app_version 
                ON app.id = app_version.app_id
             WHERE app.package = ?
             ORDER BY version DESC
             LIMIT 1
            ;
            """, [app])
        results = curs.fetchall()
        if not results:
            raise KeyError("app not in SKB")
        else:
            results = results[0][0]
            return results

if __name__ == '__main__':
    vt = VirusTotalDB()
    for x in argv:
        try:
            results = vt.query(x)
            print results
            time.sleep(16)
        except IOError, e:
            continue
