require 'optparse'
require 'yaml'

module Director
  DEFAULT_COOKIE = "454jjdllsoodh764jnd873jhey0poj"
  
  def self.generate_id
    values = [
              rand(0x0010000),
              rand(0x0010000),
              rand(0x0010000)
             ]
    "%04x%04x%04x" % values
  end
  
  class NodeRunner
    attr_accessor :options
    attr_reader :args
    
    def initialize(args)
      @args = args
      @options = {
        :daemonize => false,
        :cookie => DEFAULT_COOKIE,
        :node_name => nil,
        :phonehome => nil,
      }
      parse!
    end
    
    def daemon?
      "-detached -noinput" if @options[:daemonize]
    end
    
    def parser
      # Define opts
      @parser ||= OptionParser.new do |opts|
        opts.banner = "Usage: node [options]"

        opts.separator "Options:"
        opts.on("-A","--application DIR_NAME","REQUIRED. Root dir for your application under 'applications'"){ |dir_name| 
          @options[:app] = dir_name 
        }
        opts.on("-C","--configkey CONFIG_KEY","REQUIRED. The configuration key to use in 'director.yml'"){ |config_key| 
          @options[:config_key] = config_key 
        }
        
        opts.on("-d", "--daemonize", "run as a background process"){ @options[:daemonize] = true }
        opts.on("-c", "--cookie VALUE", "Set the Erlang cookie value (has default)"){ |value| @options[:cookie] = value }
        opts.on("-n", "--nodename NAME","Set the short name for this node (default generated)"){ |name| 
          @options[:node_name] = name
        }
      end
    end
    
    def parse!
      parser.parse! @args
    end
    
    def go!
      abort "Missing Application Directory! See 'director -help' for more information" unless @options[:app]
      
      data = YAML.load_file(File.join(DIRECTOR_ROOT,"applications",@options[:app],"config","director.yml"))
      config = data[@options[:config_key]]
      
      @options[:node_name] ||= "worker_#{Director.generate_id}"
      
      
      abort "Missing command 'type' in director.yml" unless config['type']
      
      abort "Missing 'restart' configuration in director.yml" unless config['restart']
      
      cmd = case config['type']
            when 'ruby' then do_ruby_command(config)
            when 'perl' then do_perl_command(config)  
            end
      
      #restart_strategy = config['restart']
      #puts "Try: #{restart_strategy['try']} within #{restart_strategy['seconds']}"
        
      #unless config["pidfile"] && config["command"] && config["phonehome"]
      #  abort "Missing required configuration information in 'director.yml'"
      #end
      
      #rake_command = "#{config['command']}['#{@options[:config_key]}']"
              
      #erlang_libs = %w[ebin].map do |n|
      #  "-pz #{File.join(DIRECTOR_ROOT,n)}"
      #end.join(" ") + " \\"
      
      #cmd = %Q{erl #{erlang_libs}
      #         -sname #{@options[:node_name]} \\
      #         -setcookie #{@options[:cookie]} \\
      #         -appdir #{@options[:app]} \\
      #         -pidfile #{config['pidfile']} \\
      #         -cmdname #{rake_command} \\
      #         -phonehome #{config['phonehome']} \\
      #         -boot start_sasl #{daemon?} \\
      #         -s director}.squeeze(' ')
      begin
        puts cmd
        exec cmd
      rescue 
        puts "Error"
        exit(1)
      end
    end
    
    def do_ruby_command(config)
      unless config["pidfile"] && config["command"] && config["phonehome"]
        abort "Missing required configuration information in 'director.yml'"
      end
      
      rake_command = "#{config['command']}['#{@options[:config_key]}']"
              
      erlang_libs = %w[ebin].map do |n|
        "-pz #{File.join(DIRECTOR_ROOT,n)}"
      end.join(" ") + " \\"
      
      %Q{erl #{erlang_libs}
               -sname #{@options[:node_name]} \\
               -setcookie #{@options[:cookie]} \\
               -appdir #{@options[:app]} \\
               -pidfile #{config['pidfile']} \\
               -cmdname #{rake_command} \\
               -phonehome #{config['phonehome']} \\
               -tries #{config['restart']['try']} \\
               -secs #{config['restart']['seconds']} \\
               -type "ruby" \\
               -boot start_sasl #{daemon?} \\
               -s director}.squeeze(' ')
     end
    
    def do_perl_command(config)
      
      erlang_libs = %w[ebin].map do |n|
        "-pz #{File.join(DIRECTOR_ROOT,n)}"
      end.join(" ") + " \\"
      
      %Q{erl #{erlang_libs}
               -sname #{@options[:node_name]} \\
               -setcookie #{@options[:cookie]} \\
               -appdir #{config['basedir']} \\
               -pidfile 'na' \\
               -cmdname #{config['command']} \\
               -phonehome #{config['phonehome']} \\
               -tries #{config['restart']['try']} \\
               -secs #{config['restart']['seconds']} \\
               -type "perl" \\
               -boot start_sasl #{daemon?} \\
               -s director}.squeeze(' ')
    end
    
  end # End Node Runner
  
  class ConsoleRunner
    attr_accessor :options
    attr_reader :args
    
    def initialize(args)
      @args = args
      @options = {
        :daemonize => false,
        :cookie => DEFAULT_COOKIE,
        :node_name => "director_console"
      }
      parse!
    end
    
    def daemon?
      "-detached -noinput" if @options[:daemonize]
    end
    
    def parser
      # Define opts
      @parser ||= OptionParser.new do |opts|
        opts.banner = "Usage: console [options]"

        opts.separator "Options:"
        opts.on("-d", "--daemonize", "run as a background process"){ @options[:daemonize] = true }
        opts.on("-c", "--cookie VALUE", "Set the Erlang cookie value"){ |value| @options[:cookie] = value }
        opts.on("-n", "--nodename NAME","Set the short name for this node (default: director_console)"){ |name| 
          @options[:node_name] = name
        }
      end
    end
    
    def parse!
      parser.parse! @args
    end
    
    def go!
      
      erlang_libs = %w[deps/mochiweb/ebin deps/beepbeep/ebin deps/erlydtl/ebin ebin].map do |n|
        "-pz #{File.join(DIRECTOR_ROOT,n)}"
      end.join(" ") + " \\"
      
      cmd = %Q{erl #{erlang_libs}
               -sname #{@options[:node_name]} \\
               -setcookie #{@options[:cookie]} \\
               -boot start_sasl #{daemon?} \\
               -s reloader -s director_console}.squeeze(' ')
      begin
        puts cmd
        exec cmd
      rescue 
        puts "Error"
        exit(1)
      end
    end
    
  end # End Console Runner
  
  # Work in progress. This is a command line script to stop a node without the console
  #"admin -n daveb@here.com --stop"
  class AdminConsole
    attr_accessor :options
    attr_reader :args
    
    def initialize(args)
      @args = args
      @options = {
        :cookie => DEFAULT_COOKIE,
        :node_name => "director_ctl",
        :kill => false,
        :remote_node => nil
      }
      parse!
    end
    
    def parser
      # Define opts
      @parser ||= OptionParser.new do |opts|
        opts.banner = "Usage: admin [options]"

        opts.separator "Options:"
        opts.on("-k", "--stop", "Stop the node"){ @options[:kill] = true }
        opts.on("-c", "--cookie VALUE", "Set the Erlang cookie value (default is: #{DEFAULT_COOKIE})"){ |value| @options[:cookie] = value }
        opts.on("-n", "--nodename NAME","Set the short name of the node you want to stop"){ |name| 
          @options[:remote_node] = name
        }
      end
    end
    
    def parse!
      parser.parse! @args
    end
    
    def go!
      abort "Missing the name of the Node to stop! See 'admin -help' for more information" unless @options[:remote_node]

      erlang_libs = %w[ebin].map do |n|
        "-pz #{File.join(DIRECTOR_ROOT,n)}"
      end.join(" ") + " \\"
      
      cmd = %Q{erl #{erlang_libs}
               -sname #{@options[:node_name]} \\
               -setcookie #{@options[:cookie]} \\
               -boot start_sasl \\
               -s director_ctrl -extra \'#{@options[:remote_node]}\' stop}.squeeze(' ')
      begin
        puts cmd
        exec cmd
      rescue 
        puts "Error"
        exit(1)
      end
    end
    
  end
end
