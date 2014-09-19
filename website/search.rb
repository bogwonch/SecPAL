#!/usr/bin/env ruby

require 'sinatra'
require 'sinatra/reloader'
require 'haml'

set :haml, :format => :html5

get '/' do
  haml :index do
  end
end

post '/' do
  haml :index do
    out = ""
      # params.each do |k,v|
      #   out << "<p>#{k} := #{v}</p>\n"
      # end
      # out << '<p>'
    case params['search-mode']
    when '#basic'
      out << paramsToSecPAL(params)
    when '#advanced'
      out << "<p>Oo advanced mode <emph>get you</emph></p>"
      out << "<p>#{params['advanced']}</p>"
    end
  end
end

def paramsToSecPAL(params)
  secpal = "User says app is-sought-after"
  ifs = []
  ifs += permsToSecPAL(params)

  if ifs.any?
    secpal << "\n  if\n"
    secpal << ifs.join(",\n")
  end

  conds = []
  conds << reviewToSecPAL(params)
  
  if conds.any?
    secpal << ":\n"
    secpal << conds.join(",\n")
  end

  secpal << ";"

  return secpal
end

def permsToSecPAL(params)
  ifs = []
  
  perm = permToSecPAL('perm-internet', 'android.permission.INTERNET', params)
  ifs << perm unless perm.nil?

  perm = permToSecPAL('perm-storage', 'android.permission.WRITE_EXTERNAL_STORAGE', params)
  ifs << perm unless perm.nil?

  perm = permToSecPAL('perm-iap', 'com.android.vending.BILLING', params)
  ifs << perm unless perm.nil?

  return ifs
end

def permToSecPAL(name, perm, params)
  case params[name]
  when 'yes'
    return %Q|    app has-permission("#{perm}")|
  when 'no'
    return %Q|    app doesnt-have-permission("#{perm}")|
  else
    return nil
  end
end

def reviewToSecPAL(params)
  if params['review'] == 'yes'
    case params['review-source']
    when 'google-play'
      source = 'GooglePlay'
    when 'app-brain'
      source = 'AppBrain'
    else 
      return nil
    end

    score = (params['review-stars'].to_i * 20 - 10) / 100.0
    
    return %Q|    ! reviewScore(app, #{source}) < #{score}|

  else 
    return nil
  end
end

