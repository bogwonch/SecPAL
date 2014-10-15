from flask import (Flask, request)
import sqlite3

app = Flask(__name__)
app.config['DEBUG'] = True
app.config['SECRET_KEY'] = 'CHANGEME'

# Build the connection

@app.route('/check', methods=['GET'])
def check():
    try:
        package=request.args.get('package')        
    except ValueError, e:
        return "Error: missing package or expert"

    conn = sqlite3.connect('./vFeed/vfeed.db')
    curs = conn.cursor()
    curs.execute("""
        SELECT cveid
          FROM nvd_db
         WHERE summary LIKE '%' || ? || '%';
     """, [package])

    results = curs.fetchall()
    return "\n".join([result[0] for result in results])


if __name__=='__main__':
    app.run(port=5005)
