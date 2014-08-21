from virustotaldb import (VirusTotalDB)
from flask import *

app = Flask(__name__)
app.config['DEBUG'] = True
app.config['SECRET_KEY'] = 'CHANGEME'


@app.route('/check', methods=['GET'])
def check():
    vt = VirusTotalDB()
    try:
        package=request.args.get('package')
        expert=request.args.get('expert')

        try:
            results = vt.query(package)
            malware = results[expert]
            if not malware:
                malware = "Not Malware"
                return malware
        except KeyError, e:
            return ""

    except ValueError, e:
        return "Error: missing package or expert"

@app.route('/percentage', methods=['GET'])
def percentage():
    vt = VirusTotalDB()
    try:
        package=request.args.get('package')
        return str(vt.malware_percentage(package))
    except ValueError, e:
        return "Error: missing package"

if __name__=='__main__':
    app.run(port=5002)
