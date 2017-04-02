require 'yaml'

root = File::expand_path("#{File::dirname(__FILE__)}")
data_path = "#{root}/yaml_sample.yml"

current_data = YAML.load_file(data_path)
next_data = Marshal.load(Marshal.dump(current_data))

# pretty print
puts next_data.to_yaml.gsub("\n-", "\n\n-")

