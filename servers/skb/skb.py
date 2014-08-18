#!/usr/bin/env python
# encoding: utf-8

from flask import *
import oursql

# Start a connection to the database
conn = oursql.connect(host='localhost', 
                      user='root', 
                      db='app_db', 
                      port=3306)

# Start the webserver
app = Flask(__name__)
app.config['DEBUG'] = True
app.config['SECRET_KEY'] = 'CHANGEME'

def query(sql, args=()):
    curs = conn.cursor()
    curs.execute(sql, args)
    return curs.fetchall()


@app.route('/permission', methods=['GET'])
def permission():
    try:
        package=request.args.get('package')
        permission=request.args.get('permission')

        result = query("""
            SELECT app.package, permissions.name
              FROM app
              JOIN app_perm
                ON app.id = app_perm.id_app
              JOIN permissions
                ON app_perm.id_perm = permissions.id
             WHERE app.package = ?
               AND permissions.name = ?
            ;
        """, (package, permission))

        if len(result) > 0:
            return "True"
        else:
            return "False"

    except ValueError:
        return "Error: missing package or permission name"


@app.route('/category', methods=['GET'])
def category():
    try: 
        package=request.args.get('package')
    
        result = query("""
            SELECT app.category
              FROM app
             WHERE app.package = ?
            ;
        """, [package])

        if len(result) > 0:
            category = result[0][0]
            if category is None:
                return '"UNKNOWN"'
            else:
                return('"%s"' % category)
        else:
            return '"UNKNOWN"'

    except ValueError:
        return "Error: missing package"


if __name__=='__main__':
    app.run(port=5001)
