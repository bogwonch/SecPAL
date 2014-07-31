#!/usr/bin/env python
# encoding: utf-8

from flask import *
from rpy2.robjects.packages import importr
from rpy2.rinterface import NULL
import rpy2.robjects as ro

r = ro.r
arules=importr('arules')
r.source('mba.r')
installs=r.installs
removals=r.removals

app = Flask(__name__)
app.config['DEBUG'] = True
app.config['SECRET_KEY'] = 'SUCHSECRET'

@app.route('/support', methods=['GET'])
def support():
    try:
        lhs=request.args.get('lhs')
        rhs=request.args.get('rhs')
        support=r['install.lift'](lhs,rhs)
        if support != NULL:
            return str(support[0])
        else:
            return '0'
    except ValueError:
        return "Error: missing arguments lhs and rhs"

@app.route('/confidence', methods=['GET'])
def confidence():
    try:
        lhs=request.args.get('lhs')
        rhs=request.args.get('rhs')
        confidence=r['install.lift'](lhs,rhs)
        if confidence != NULL:
            return str(confidence[0])
        else:
            return '0'
    except ValueError:
        return "Error: missing arguments lhs and rhs"

@app.route('/lift', methods=['GET'])
def lift():
    try:
        lhs=request.args.get('lhs')
        rhs=request.args.get('rhs')
        lift=r['install.lift'](lhs,rhs)
        if lift != NULL:
            return str(lift[0])
        else:
            return '0'
    except ValueError:
        return "Error: missing arguments lhs and rhs"




@app.route( '/greet', methods=['GET'] )
def greet():
    who = request.args.get('who','bob')
    return 'hello '+who

if __name__=='__main__':
    app.run()
