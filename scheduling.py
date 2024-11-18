def find_min_time(load_pairs):
    """
    Find the load with the minimum wash or dry time in a list of wash-dry pairs.

    Args:
        load_pairs: List of tuples where each tuple is (wash_time, dry_time).

    Returns:
        A tuple (min_time, min_index, is_wash), where:
        - min_time is the smallest time value (wash or dry time).
        - min_index is the index of the load with the smallest time.
        - is_wash is True if the smallest time is a wash time, False if it's a dry time.
    """
    min_time = None
    min_index = None
    is_wash = None

    for index, (wash_time, dry_time) in enumerate(load_pairs):
        if min_time is None or wash_time < min_time:
            min_time = wash_time
            min_index = index
            is_wash = True
        if dry_time < min_time:
            min_time = dry_time
            min_index = index
            is_wash = False

    return min_index, is_wash


def calculate_cumulative_time(load_pairs):
    """
    Calculate cumulative times for washing and drying with one washer and one dryer.

    Args:
        load_pairs: List of tuples where each tuple is (wash_time, dry_time).

    Returns:
        A list of tuples (wash_start_time, wash_end_time, dry_start_time, dry_end_time) for each load.
    """
    cumulative_times = []
    current_wash_end = 0  # When the washer is free
    current_dry_end = 0  # When the dryer is free

    for wash_time, dry_time in load_pairs:
        wash_end = current_wash_end + wash_time  # Time when washing ends
        dry_start = max(wash_end, current_dry_end)  # Dryer starts after washing ends
        dry_end = dry_start + dry_time  # Time when drying ends

        cumulative_times.append((current_wash_end, wash_end, dry_start, dry_end))

        current_wash_end = wash_end
        current_dry_end = dry_end

    return cumulative_times


def print_gantt_chart(load_pairs):
    """
    Print a Gantt chart for wash-dry schedules.

    Args:
        load_pairs: List of tuples where each tuple is (wash_time, dry_time).
    """
    print("=" * 42)
    print("              GANTT CHART")
    print("=" * 42)

    # Calculate cumulative times
    cumulative_times = calculate_cumulative_time(load_pairs)

    # Washer schedule
    print("\nWASHER SCHEDULE:\n" + "-" * 42)
    for i, (wash_start, wash_end, _, _) in enumerate(cumulative_times):
        print(f"Load {i + 1}: |{wash_start}-{wash_end}| ({wash_end - wash_start} minutes)")

    # Dryer schedule
    print("\nDRYER SCHEDULE:\n" + "-" * 42)
    for i, (_, _, dry_start, dry_end) in enumerate(cumulative_times):
        print(f"Load {i + 1}: |{dry_start}-{dry_end}| ({dry_end - dry_start} minutes)")


def optimize_laundry(loads, start_order=None, end_order=None):
    """
    Recursively optimize laundry order by scheduling loads based on the smallest time (wash or dry).

    Args:
        loads: List of tuples where each tuple is (wash_time, dry_time).
        start_order: List of selected loads for the washer (initially empty).
        end_order: List of selected loads for the dryer (initially empty).
    """
    if start_order is None:
        start_order = []
    if end_order is None:
        end_order = []

    if not loads:
        print_gantt_chart(start_order + end_order)  # Base case: Print the Gantt chart
        return

    min_index, is_wash = find_min_time(loads)
    selected_load = loads[min_index]
    remaining_loads = loads[:min_index] + loads[min_index + 1:]

    if is_wash:
        optimize_laundry(remaining_loads, start_order + [selected_load], end_order)
    else:
        optimize_laundry(remaining_loads, start_order, [selected_load] + end_order)


def test_laundry_scheduler():
    """
    Test the laundry scheduler with sample data.
    """
    loads = [(30, 25), (2, 15), (45, 40)]
    print("Input loads (wash_time, dry_time):")
    for wash_time, dry_time in loads:
        print(f"Wash: {wash_time}, Dry: {dry_time}")
    print("\nScheduling and generating Gantt chart...\n")
    optimize_laundry(loads)


# Run the test
test_laundry_scheduler()
