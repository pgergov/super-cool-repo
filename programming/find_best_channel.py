from prettytable import PrettyTable
import subprocess


def get_all_wlan_channels_info():
    ps1 = subprocess.Popen((['sudo', 'iwlist', 'wlan0', 'scan']), stdout=subprocess.PIPE)
    ps2 = subprocess.Popen(('grep', 'Frequency'), stdin=ps1.stdout, stdout=subprocess.PIPE)
    ps3 = subprocess.Popen(('sort'), stdin=ps2.stdout, stdout=subprocess.PIPE)
    ps4 = subprocess.Popen(('uniq', '-c'), stdin=ps3.stdout, stdout=subprocess.PIPE)

    try:
        data, error = ps4.communicate()
        return data.decode('utf-8')

    except Exception as e:
        raise e


def occurrences_to_percent(*, occurrences, total):
    return occurrences * 100 // total


def info_to_channel_times_dict(channels_info):
    result = []
    channel_info_lines = channels_info.split('\n')

    for line in channel_info_lines:
        if line == '':
            continue

        # Example line: '      3                     Frequency:2.412 GHz (Channel 1)'
        elements = line.split()

        occurrences = int(elements[0])
        channel = int("".join([numb for numb in elements[-1] if numb.isdigit()]))

        result.append({'channel': channel, 'times': occurrences})

    return result


def info_to_statistics(channels_info):
    channels_info = info_to_channel_times_dict(channels_info)
    total_channels_used = sum(x['times'] for x in channels_info)

    for data in channels_info:
        yield dict(data, **{'percent': occurrences_to_percent(occurrences=data['times'],
                                                              total=total_channels_used)})


def generate_statistics_table(statistics):
    row_id = 1
    table = PrettyTable(["#", "Channel", "Times used", "%"])

    for data in statistics:
        table.add_row([row_id, data.get('channel'), data.get('times'), data.get('percent')])
        row_id += 1

    return table


def main():
    channels_info = get_all_wlan_channels_info()
    statistics = info_to_statistics(channels_info)
    table = generate_statistics_table(statistics)
    print(table)

if __name__ == '__main__':
    main()
