#!/usr/bin/env ruby

require 'haml'
require 'httparty'
require 'httparty/response'
require 'json'
require 'nokogiri'
require 'open-uri'
require 'sinatra'
require 'sinatra/reloader'

set :haml, :format => :html5

SECPAL_SERVER="http://localhost:2345/"

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
    secpal = nil
    case params['search-mode']
    when '#basic'
      secpal = paramsToSecPAL(params)
    when '#advanced'
      secpal = params['advanced'].split(';')
      secpal.map! {|v| "#{v};"}
    end
      
    unless secpal.nil?
      response = HTTParty.post(SECPAL_SERVER, 
                               body: {localContext: secpal, 
                                      query:":for apk#app User says apk#app is-sought-after;"
                                     }.to_json,
                               headers: { 'Content-Type' => 'application/json',
                                          'Accept' => 'application/json'
                                        }

                              )

      unless response.success?
        out << "<h1>uh oh... something went wrong</h1>"
      end
      results = JSON.parse(response, symbolize_names: true)
      results.each do |result|
        if result[:result] == 'True'
          apk = (/^User says apk#(.+) is-sought-after\(\);$/.match result[:query])[1].downcase

          uri = "https://play.google.com/store/apps/details?id=#{apk}"
          img = getImage(apk)
          out << <<-eof
          <button class="result btn btn-default">
            <a href="#{uri}">
              <img src="#{img}" width="80%" height="80%"/>
            </a>
            <div class="result-desc">
              #{apk} 
            </div>
          </button>
          eof


        end
      end
    end
    out
  end
end

def getImage(apk)
  if File.exists? "public/img/#{apk}.png"
    return "/img/#{apk}.png"
  else
    fork do
      uri = "https://play.google.com/store/apps/details?id=#{apk}"
      begin
        page = Nokogiri::HTML(open(uri))
        img = page.css('div.details-info img.cover-image').attribute("src")
      rescue Exception 
        return 
      end

      open("public/img/#{apk}.png", "wb") do |src|
        src << open(img).read
      end
    end

    return "/img/#{apk}.png"
  end
end

def paramsToSecPAL(params)
  ifs = []
  ifs += permsToSecPAL(params)

  conds = []
  conds << reviewToSecPAL(params)
  
  if ifs.any? or conds.any?
    secpal = "User says app is-sought-after"
  
    if ifs.any?
      secpal << "\n  if\n"
      secpal << ifs.join(",\n")
    end

    if conds.any?
      secpal << ":\n"
      secpal << conds.join(",\n")
    end
    
    secpal << ";"

    return [secpal]
  else
    return ["User says app is-sought-after if app is-an-app;"]
  end
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

